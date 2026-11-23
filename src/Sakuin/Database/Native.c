/* Native reader for nix-sakuin's FrCode version 1. */
#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utf8proc.h>
#include <zstd.h>
#ifdef __APPLE__
#include <xlocale.h>
#endif

enum { Regex = 1, AtRoot = 2, WholeName = 4, Minimal = 8 };
enum { ReadSize = 65536 };
static const unsigned char magic[] = "SAKUIN-FRCODE\1\n";

typedef struct {
    const unsigned char *data;
    size_t size;
} Bytes;

typedef struct {
    unsigned char *data;
    size_t size;
    size_t capacity;
} Buffer;

static Bytes view(const Buffer *buffer) { return (Bytes){buffer->data, buffer->size}; }

static Bytes string_bytes(const char *string)
{
    return (Bytes){(const unsigned char *)string, strlen(string)};
}

static Bytes drop(Bytes bytes, size_t count)
{
    return (Bytes){bytes.data + count, bytes.size - count};
}

static bool starts_with(Bytes bytes, Bytes prefix)
{
    return prefix.size <= bytes.size &&
           (prefix.size == 0 || memcmp(bytes.data, prefix.data, prefix.size) == 0);
}

static bool reserve(Buffer *buffer, size_t size)
{
    if (size <= buffer->capacity)
        return true;

    size_t capacity = buffer->capacity ? buffer->capacity : 256;
    while (capacity < size)
        capacity = capacity <= SIZE_MAX / 2 ? capacity * 2 : size;

    void *data = realloc(buffer->data, capacity);
    if (!data)
        return false;
    buffer->data = data;
    buffer->capacity = capacity;
    return true;
}

static bool append(Buffer *buffer, Bytes bytes)
{
    if (bytes.size >= SIZE_MAX - buffer->size || !reserve(buffer, buffer->size + bytes.size + 1))
        return false;
    if (bytes.size)
        memcpy(buffer->data + buffer->size, bytes.data, bytes.size);
    buffer->size += bytes.size;
    buffer->data[buffer->size] = 0;
    return true;
}

typedef struct {
    FILE *file;
    ZSTD_DStream *stream;
    unsigned char compressed[ReadSize];
    ZSTD_inBuffer input;
    Buffer decoded;
    size_t offset;
    size_t remaining;
    bool eof;
    const char *error;
} Reader;

/* Append decompressed bytes, retaining the current unfinished record. */
static int read_more(Reader *reader)
{
    Buffer *output = &reader->decoded;
    if (reader->offset) {
        output->size -= reader->offset;
        memmove(output->data, output->data + reader->offset, output->size);
        reader->offset = 0;
    }
    if (output->size > SIZE_MAX - ReadSize || !reserve(output, output->size + ReadSize)) {
        reader->error = "out of memory while reading FrCode";
        return -1;
    }

    for (;;) {
        if (!reader->eof && reader->input.pos == reader->input.size) {
            size_t length = fread(reader->compressed, 1, sizeof reader->compressed, reader->file);
            if (!length && ferror(reader->file)) {
                reader->error = "could not read FrCode database";
                return -1;
            }
            reader->input = (ZSTD_inBuffer){reader->compressed, length, 0};
            reader->eof = length == 0;
        }
        if (reader->eof && reader->remaining == 0)
            return 0;

        ZSTD_outBuffer next = {output->data + output->size, output->capacity - output->size, 0};
        reader->remaining = ZSTD_decompressStream(reader->stream, &next, &reader->input);
        if (ZSTD_isError(reader->remaining)) {
            reader->error = ZSTD_getErrorName(reader->remaining);
            return -1;
        }
        if (next.pos) {
            output->size += next.pos;
            return 1;
        }
        if (reader->eof && reader->remaining != 0) {
            reader->error = "truncated zstd frame";
            return -1;
        }
    }
}

static int require_bytes(Reader *reader, size_t count)
{
    while (reader->decoded.size - reader->offset < count) {
        int status = read_more(reader);
        if (status <= 0)
            return status;
    }
    return 1;
}

/* Offsets are relative to the current record and survive buffer compaction. */
static int find_byte(Reader *reader, size_t start, unsigned char byte, size_t *offset)
{
    for (;;) {
        size_t available = reader->decoded.size - reader->offset;
        if (start < available) {
            const unsigned char *record = reader->decoded.data + reader->offset;
            const unsigned char *found = memchr(record + start, byte, available - start);
            if (found) {
                *offset = (size_t)(found - record);
                return 1;
            }
        }
        start = available;
        int status = read_more(reader);
        if (status <= 0)
            return status;
    }
}

typedef struct {
    Bytes metadata;
    Bytes suffix;
    int differential;
} Record;

static int read_record(Reader *reader, Record *entry)
{
    int status = require_bytes(reader, 1);
    if (status <= 0)
        return status;

    size_t separator, newline;
    if (find_byte(reader, 0, 0, &separator) != 1)
        goto truncated;

    size_t start = separator + 1;
    if (require_bytes(reader, start + 1) != 1)
        goto truncated;

    unsigned first = reader->decoded.data[reader->offset + start++];
    int differential = first < 128 ? (int)first : (int)first - 256;
    if (first == 128) {
        if (require_bytes(reader, start + 2) != 1)
            goto truncated;
        const unsigned char *delta = reader->decoded.data + reader->offset + start;
        int wide = 256 * delta[0] + delta[1];
        differential = wide < 32768 ? wide : wide - 65536;
        start += 2;
    }
    if (find_byte(reader, start, '\n', &newline) != 1)
        goto truncated;

    const unsigned char *data = reader->decoded.data + reader->offset;
    *entry = (Record){{data, separator}, {data + start, newline - start}, differential};
    reader->offset += newline + 1;
    return 1;

truncated:
    if (!reader->error)
        reader->error = "truncated FrCode record";
    return -1;
}

static const char *unescape(Buffer *output, Bytes bytes)
{
    output->size = 0;
    if (!append(output, bytes))
        return "out of memory while unescaping FrCode";

    if (!memchr(bytes.data, 255, bytes.size))
        return NULL;

    size_t written = 0;
    for (size_t i = 0; i < bytes.size; i++) {
        unsigned char byte = bytes.data[i];
        if (byte == 255) {
            if (++i == bytes.size || (bytes.data[i] != 1 && bytes.data[i] != 11))
                return "invalid escaped byte in FrCode";
            byte = bytes.data[i] - 1;
        }
        output->data[written++] = byte;
    }
    output->size = written;
    output->data[written] = 0;
    return NULL;
}

static bool is_ascii(Bytes bytes)
{
    while (bytes.size >= sizeof(uint64_t)) {
        uint64_t word;
        memcpy(&word, bytes.data, sizeof word);
        if (word & UINT64_C(0x8080808080808080))
            return false;
        bytes = drop(bytes, sizeof word);
    }
    for (size_t i = 0; i < bytes.size; i++)
        if (bytes.data[i] >= 128)
            return false;
    return true;
}

static unsigned char ascii_lower(unsigned char byte)
{
    return byte >= 'A' && byte <= 'Z' ? byte + ('a' - 'A') : byte;
}

static const char *case_fold(Buffer *output, Bytes input, Bytes *result)
{
    size_t i = 0;
    while (i < input.size && input.data[i] < 128 && !(input.data[i] >= 'A' && input.data[i] <= 'Z'))
        i++;
    if (i == input.size) {
        *result = input;
        return NULL;
    }

    output->size = 0;
    if (!append(output, input))
        return "out of memory while folding search text";
    for (; i < input.size; i++) {
        unsigned char byte = input.data[i];
        if (byte >= 128) {
            if (input.size > PTRDIFF_MAX)
                return "search text exceeds the Unicode library's length limit";
            utf8proc_uint8_t *folded = NULL;
            utf8proc_ssize_t size =
                utf8proc_map(input.data, (utf8proc_ssize_t)input.size, &folded, UTF8PROC_CASEFOLD);
            if (size < 0)
                return utf8proc_errmsg(size);
            free(output->data);
            *output = (Buffer){folded, (size_t)size, (size_t)size + 1};
            break;
        }
        if (byte >= 'A' && byte <= 'Z')
            output->data[i] = byte + ('a' - 'A');
    }
    *result = view(output);
    return NULL;
}

/* Find the first hash-shaped component, using the same boundary as locate. */
static bool find_store_base(Bytes path, size_t *end, Bytes *hash)
{
    static const char alphabet[] = "0123456789abcdfghijklmnpqrsvwxyz";
    for (size_t start = 0; start < path.size;) {
        const unsigned char *component = path.data + start;
        const unsigned char *slash = memchr(component, '/', path.size - start);
        size_t length = slash ? (size_t)(slash - component) : path.size - start;
        const unsigned char *dash = memchr(component, '-', length);
        if (dash) {
            size_t hash_size = (size_t)(dash - component);
            size_t characters = 0;
            for (size_t i = 0; i < hash_size; i++)
                characters += (component[i] & 0xc0) != 0x80;
            if (characters == 32) {
                if (hash_size != 32 || length < 34)
                    return false;
                for (size_t i = 0; i < 32; i++)
                    if (!component[i] || !strchr(alphabet, component[i]))
                        return false;
                *end = start + length;
                *hash = (Bytes){component, 32};
                return true;
            }
        }
        start += length + 1;
    }
    return false;
}

typedef struct {
    unsigned char *name;
    size_t size;
    uint64_t hash;
} SeenEntry;

typedef struct {
    SeenEntry *entries;
    size_t capacity;
    size_t count;
} Seen;

static uint64_t name_hash(Bytes name)
{
    uint64_t hash = UINT64_C(14695981039346656037);
    for (size_t i = 0; i < name.size; i++)
        hash = (hash ^ name.data[i]) * UINT64_C(1099511628211);
    return hash;
}

static SeenEntry *seen_slot(const Seen *seen, Bytes name, uint64_t hash)
{
    size_t index = (size_t)hash & (seen->capacity - 1);
    for (;;) {
        SeenEntry *entry = &seen->entries[index];
        if (!entry->name || (entry->hash == hash && entry->size == name.size &&
                             memcmp(entry->name, name.data, name.size) == 0))
            return entry;
        index = (index + 1) & (seen->capacity - 1);
    }
}

static bool mark_seen(Seen *seen, Bytes name)
{
    if (seen->count >= seen->capacity / 2) {
        if (seen->capacity > SIZE_MAX / (2 * sizeof(SeenEntry)))
            return false;
        size_t capacity = seen->capacity ? 2 * seen->capacity : 64;
        Seen grown = {calloc(capacity, sizeof(SeenEntry)), capacity, seen->count};
        if (!grown.entries)
            return false;
        for (size_t i = 0; i < seen->capacity; i++) {
            SeenEntry entry = seen->entries[i];
            if (entry.name)
                *seen_slot(&grown, (Bytes){entry.name, entry.size}, entry.hash) = entry;
        }
        free(seen->entries);
        *seen = grown;
    }

    unsigned char *copy = malloc(name.size + 1);
    if (!copy)
        return false;
    memcpy(copy, name.data, name.size);
    uint64_t hash = name_hash(name);
    *seen_slot(seen, name, hash) = (SeenEntry){copy, name.size, hash};
    seen->count++;
    return true;
}

typedef struct {
    const char *package_prefix;
    const char *hash_filter;
    const char *file_types;
    int flags;
    regex_t regex;
    bool regex_ready;
    Buffer path, package, store, unescaped, full, folded, needle;
    Bytes pattern;
    Seen seen;
    bool reject_package;
    bool canonical;
    bool matching_hash;
} Query;

static bool exact_root_literal(const Query *query)
{
    return (query->flags & (Regex | AtRoot | WholeName)) == (AtRoot | WholeName);
}

static bool matching_hash(const Query *query, Bytes hash)
{
    return !query->hash_filter || (strlen(query->hash_filter) == hash.size &&
                                   memcmp(query->hash_filter, hash.data, hash.size) == 0);
}

static const char *prepare_package(Query *query, Bytes metadata)
{
    const char *error = unescape(&query->package, drop(metadata, 1));
    if (!error)
        error = unescape(&query->store, view(&query->path));
    if (error)
        return error;

    Bytes package = view(&query->package);
    if (query->package_prefix && !starts_with(package, string_bytes(query->package_prefix))) {
        query->reject_package = true;
        return NULL;
    }
    query->reject_package =
        query->seen.capacity && seen_slot(&query->seen, package, name_hash(package))->name;
    if (query->reject_package)
        return NULL;

    Bytes store = view(&query->store), hash;
    size_t end;
    query->canonical = find_store_base(store, &end, &hash) && end == store.size;
    query->matching_hash = query->canonical && matching_hash(query, hash);
    if (query->canonical && exact_root_literal(query)) {
        Bytes folded;
        error = case_fold(&query->folded, store, &folded);
        if (error)
            return error;
        query->canonical = find_store_base(folded, &end, &hash) && end == folded.size;
    }
    return NULL;
}

static bool literal_prefix(Bytes path, Bytes pattern, bool ascii)
{
    if (!ascii)
        return starts_with(path, pattern);
    if (pattern.size > path.size)
        return false;
    for (size_t i = 0; i < pattern.size; i++)
        if (ascii_lower(path.data[i]) != pattern.data[i])
            return false;
    return true;
}

static bool contains_from(Bytes path, Bytes pattern, bool ascii, unsigned char first)
{
    while (path.size >= pattern.size) {
        size_t candidates = path.size - pattern.size + 1;
        const unsigned char *start = memchr(path.data, first, candidates);
        if (!start)
            return false;
        path = drop(path, (size_t)(start - path.data));
        if (literal_prefix(path, pattern, ascii))
            return true;
        path = drop(path, 1);
    }
    return false;
}

static bool contains(Bytes path, Bytes pattern, bool ascii)
{
    if (pattern.size == 0)
        return true;
    unsigned char first = pattern.data[0];
    return contains_from(path, pattern, ascii, first) ||
           (ascii && first >= 'a' && first <= 'z' &&
            contains_from(path, pattern, true, first - ('a' - 'A')));
}

static const char *matches_path(Query *query, Bytes relative, bool ascii, bool *matched)
{
    Bytes path = relative;
    *matched = false;
    if (query->canonical && relative.size && relative.data[0] == '/') {
        if (!query->matching_hash)
            return NULL;
    } else {
        if (query->canonical && relative.size == 0)
            return NULL;
        query->full.size = 0;
        if (!append(&query->full, view(&query->store)) || !append(&query->full, relative))
            return "out of memory while building store path";
        path = view(&query->full);
        size_t end;
        Bytes hash;
        if (query->hash_filter && (!find_store_base(path, &end, &hash) || end == path.size ||
                                   !matching_hash(query, hash)))
            return NULL;
        if (exact_root_literal(query)) {
            const char *error = case_fold(&query->folded, path, &path);
            if (error)
                return error;
        }
        if (!find_store_base(path, &end, &hash) || end == path.size)
            return NULL;
        path = drop(path, end);
        if (exact_root_literal(query)) {
            *matched = path.size == query->pattern.size && starts_with(path, query->pattern);
            return NULL;
        }
        ascii = !(query->flags & Regex) && is_ascii(path);
    }

    if (query->flags & Regex) {
        /* regoff_t is int on some supported libcs. */
        if (path.size > INT_MAX)
            return "path exceeds the native regex engine's length limit";
        regmatch_t range = {.rm_so = 0, .rm_eo = (regoff_t)path.size};
        int status = regexec(&query->regex, (const char *)path.data, 1, &range, REG_STARTEND);
        if (status != 0 && status != REG_NOMATCH)
            return "native regex matching failed";
        *matched = status == 0;
        return NULL;
    }

    if (!ascii) {
        const char *error = case_fold(&query->folded, path, &path);
        if (error)
            return error;
    }
    Bytes pattern = query->pattern;
    if (query->flags & AtRoot)
        *matched = literal_prefix(path, pattern, ascii) &&
                   (!(query->flags & WholeName) || path.size == pattern.size);
    else if (query->flags & WholeName)
        *matched = pattern.size <= path.size &&
                   literal_prefix(drop(path, path.size - pattern.size), pattern, ascii);
    else
        *matched = contains(path, pattern, ascii);
    return NULL;
}

static bool write_bytes(Bytes bytes)
{
    return !bytes.size || fwrite(bytes.data, 1, bytes.size, stdout) == bytes.size;
}

static const char *search_file(Query *query, Bytes metadata)
{
    Bytes path = view(&query->path);
    /* An ASCII path cannot contain the FrCode escape byte (0xff). */
    bool ascii = !(query->flags & Regex) && is_ascii(path);
    if (!ascii && memchr(path.data, 255, path.size)) {
        const char *error = unescape(&query->unescaped, path);
        if (error)
            return error;
        path = view(&query->unescaped);
    }

    unsigned char type = metadata.data[metadata.size - 1];
    if (query->reject_package ||
        (*query->file_types && (!type || !strchr(query->file_types, type))))
        return NULL;

    bool matched;
    const char *error = matches_path(query, path, ascii, &matched);
    if (error || !matched)
        return error;

    Bytes package = view(&query->package);
    if (query->flags & Minimal) {
        if (!mark_seen(&query->seen, package))
            return "out of memory while deduplicating package names";
        query->reject_package = true;
        if (write_bytes(package) && fputc('\n', stdout) != EOF)
            return NULL;
    } else if (write_bytes(package) && fputc('\t', stdout) != EOF && write_bytes(metadata) &&
               fputc('\t', stdout) != EOF && write_bytes(view(&query->store)) &&
               write_bytes(path) && fputc('\n', stdout) != EOF) {
        return NULL;
    }
    return "could not write query output";
}

static const char *scan(Reader *reader, Query *query)
{
    if (require_bytes(reader, sizeof magic - 1) != 1 ||
        memcmp(reader->decoded.data + reader->offset, magic, sizeof magic - 1) != 0)
        return reader->error ? reader->error : "invalid FrCode header or unsupported version";
    reader->offset += sizeof magic - 1;

    bool expect_package = true;
    int shared = 0, status;
    Record entry;
    while ((status = read_record(reader, &entry)) == 1) {
        shared += entry.differential;
        if (shared < 0 || shared > INT16_MAX || (size_t)shared > query->path.size)
            return "FrCode shared prefix is out of range";
        query->path.size = (size_t)shared;
        if (!append(&query->path, entry.suffix))
            return "out of memory while expanding FrCode path";

        if (expect_package) {
            if (!entry.metadata.size || entry.metadata.data[0] != 'p')
                return "expected a FrCode package record";
            const char *error = prepare_package(query, entry.metadata);
            if (error)
                return error;
            expect_package = false;
        } else if (entry.metadata.size == 0 && query->path.size == 0) {
            expect_package = true;
        } else {
            if (!entry.metadata.size || entry.metadata.data[0] < '0' ||
                entry.metadata.data[0] > '9')
                return "invalid FrCode file metadata";
            const char *error = search_file(query, entry.metadata);
            if (error)
                return error;
        }
    }

    if (status < 0)
        return reader->error;
    return expect_package ? NULL : "missing FrCode package footer";
}

static bool prepare_pattern(Query *query, const char *pattern)
{
    bool anchored = query->flags & (AtRoot | WholeName);
    if (query->flags & Regex) {
        if (anchored && !append(&query->needle, string_bytes(query->flags & AtRoot ? "^(" : "(")))
            return false;
        if (anchored && *pattern != '/' && !append(&query->needle, string_bytes("/")))
            return false;
        return append(&query->needle, string_bytes(pattern)) &&
               (!anchored ||
                append(&query->needle, string_bytes(query->flags & WholeName ? ")$" : ")")));
    }
    if ((query->flags & AtRoot) && *pattern != '/' && !append(&query->needle, string_bytes("/")))
        return false;
    return append(&query->needle, string_bytes(pattern));
}

int sakuin_frcode_query(const char *filename, const char *pattern, const char *package_prefix,
                        const char *hash_filter, const char *file_types, int flags, char *message,
                        size_t message_size)
{
    Reader reader = {.remaining = 1};
    Query query = {.package_prefix = package_prefix,
                   .hash_filter = hash_filter,
                   .file_types = file_types,
                   .flags = flags};
    const char *error = NULL;
    int status = 1;
    locale_t locale = (locale_t)0, prior_locale = (locale_t)0;

    if (!prepare_pattern(&query, pattern)) {
        error = "out of memory while preparing search pattern";
        goto done;
    }
    if (flags & Regex) {
        locale = newlocale(LC_CTYPE_MASK, "C.UTF-8", (locale_t)0);
        if (!locale)
            locale = newlocale(LC_CTYPE_MASK, "en_US.UTF-8", (locale_t)0);
        if (locale)
            prior_locale = uselocale(locale);
        int result =
            regcomp(&query.regex, (const char *)query.needle.data, REG_EXTENDED | REG_NEWLINE);
        if (result != 0) {
            regerror(result, &query.regex, message, message_size);
            status = 2;
            goto done;
        }
        query.regex_ready = true;
    } else {
        error = case_fold(&query.folded, view(&query.needle), &query.pattern);
        if (error)
            goto done;
        /* The folded pattern must outlive the per-file folding scratch buffer. */
        if (query.pattern.data == query.folded.data) {
            free(query.needle.data);
            query.needle = query.folded;
            query.folded = (Buffer){0};
        }
    }

    reader.file = fopen(filename, "rb");
    if (!reader.file) {
        error = strerror(errno);
        goto done;
    }
    reader.stream = ZSTD_createDStream();
    if (!reader.stream) {
        error = "could not allocate zstd stream";
        goto done;
    }
    reader.remaining = ZSTD_initDStream(reader.stream);
    if (ZSTD_isError(reader.remaining)) {
        error = ZSTD_getErrorName(reader.remaining);
        goto done;
    }
    error = scan(&reader, &query);
    if (!error && fflush(stdout) != 0)
        error = "could not flush query output";
    if (!error)
        status = 0;

done:
    if (error)
        snprintf(message, message_size, "FrCode C query: %s", error);
    if (query.regex_ready)
        regfree(&query.regex);
    if (prior_locale)
        uselocale(prior_locale);
    if (locale)
        freelocale(locale);
    if (reader.file)
        fclose(reader.file);
    ZSTD_freeDStream(reader.stream);
    free(reader.decoded.data);
    free(query.path.data);
    free(query.package.data);
    free(query.store.data);
    free(query.unescaped.data);
    free(query.full.data);
    free(query.folded.data);
    free(query.needle.data);
    for (size_t i = 0; i < query.seen.capacity; i++)
        free(query.seen.entries[i].name);
    free(query.seen.entries);
    return status;
}
