type semver = Semver of int * int * int
type query = QueryPatch of int * int * int
           | QueryMinor of int * int
           | QueryMajor of int
type version_part = [
  `Major
| `Minor
| `Patch
]

val compare_version : semver -> semver -> int

val increment_version : version_part -> semver -> semver
val decrement_version : version_part -> semver -> semver

val parse_version : string -> semver
val print_version : semver -> string

val query_version : query -> semver list -> semver option

val parse_query : string -> query
val print_query : query -> string

val query : string -> semver list -> semver option
