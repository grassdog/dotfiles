# Bash Booster 0.5beta <http://www.bashbooster.net>
# =================================================
#
# Copyright (c) 2014, Dmitry Vakhrushev <self@kr41.net> and Contributors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

##
# ./source/00_error.sh
#

BB_ERROR_ASSERT_FAILED=10
BB_ERROR_WORKSPACE_CREATION_FAILED=11
BB_ERROR_EVENT_MAX_DEPTH_REACHED=12
BB_ERROR_TASK_BAD_CONTEXT=13
BB_ERROR_TASK_UNDEFINED=14

BB_ERROR=0

bb-error?() {
    BB_ERROR=$?
    (( BB_ERROR != 0 ))
}

##
# ./source/01_var.sh
#

bb-var() {
    local VAR_NAME=$1
    local DEFAULT=$2
    if [[ -z "${!VAR_NAME}" ]]
    then
        eval "$VAR_NAME='$DEFAULT'"
    fi
}

##
# ./source/02_log.sh
#

BB_LOG_DEBUG=1
BB_LOG_INFO=2
BB_LOG_WARNING=3
BB_LOG_ERROR=4

declare -A BB_LOG_LEVEL_NAME
BB_LOG_LEVEL_NAME[$BB_LOG_DEBUG]='DEBUG'
BB_LOG_LEVEL_NAME[$BB_LOG_INFO]='INFO'
BB_LOG_LEVEL_NAME[$BB_LOG_WARNING]='WARNING'
BB_LOG_LEVEL_NAME[$BB_LOG_ERROR]='ERROR'

declare -A BB_LOG_LEVEL_CODE
BB_LOG_LEVEL_CODE['DEBUG']=$BB_LOG_DEBUG
BB_LOG_LEVEL_CODE['INFO']=$BB_LOG_INFO
BB_LOG_LEVEL_CODE['WARNING']=$BB_LOG_WARNING
BB_LOG_LEVEL_CODE['ERROR']=$BB_LOG_ERROR

bb-var BB_LOG_LEVEL $BB_LOG_INFO
bb-var BB_LOG_PREFIX "$( basename "$0" )"
bb-var BB_LOG_TIME 'date +"%Y-%m-%d %H:%M:%S"'
bb-var BB_LOG_FORMAT '${PREFIX} [${LEVEL}] ${MESSAGE}'
bb-var BB_LOG_USE_COLOR false

$BB_LOG_USE_COLOR && BB_LOG_FORMAT="\${COLOR}${BB_LOG_FORMAT}\${NOCOLOR}"

bb-var BB_LOG_FORMAT "$BB_LOG_DEFAULT_FORMAT"

declare -A BB_LOG_COLORS
BB_LOG_COLORS[$BB_LOG_DEBUG]="$( tput bold )$( tput setaf 0 )"  # Dark Gray
BB_LOG_COLORS[$BB_LOG_INFO]="$( tput setaf 2 )"                 # Green
BB_LOG_COLORS[$BB_LOG_WARNING]="$( tput setaf 3 )"              # Brown/Orange
BB_LOG_COLORS[$BB_LOG_ERROR]="$( tput setaf 1 )"                # Red
BB_LOG_COLORS['NC']="$( tput sgr0 )"

bb-log-level-code() {
    local CODE=$(( $BB_LOG_LEVEL ))
    if (( $CODE == 0 ))
    then
        CODE=$(( ${BB_LOG_LEVEL_CODE[$BB_LOG_LEVEL]} ))
    fi
    echo $CODE
}

bb-log-level-name() {
    local NAME="$BB_LOG_LEVEL"
    if (( $BB_LOG_LEVEL != 0 ))
    then
        NAME="${BB_LOG_LEVEL_NAME[$BB_LOG_LEVEL]}"
    fi
    echo $NAME
}

bb-log-prefix() {
    local PREFIX="$BB_LOG_PREFIX"
    local i=2
    while echo "${FUNCNAME[$i]}" | grep -q '^bb-log' || \
          [[ "${FUNCNAME[$i]}" == 'bb-exit' ]] || \
          [[ "${FUNCNAME[$i]}" == 'bb-cleanup' ]]
    do
        i=$(( $i + 1 ))
    done
    if echo "${FUNCNAME[$i]}" | grep -q '^bb-'
    then
        PREFIX=$( echo "${FUNCNAME[$i]}" | awk '{ split($0, PARTS, "-"); print PARTS[1]"-"PARTS[2] }' )
    fi
    echo "$PREFIX"
}

bb-log-msg() {
    local LEVEL_CODE=$(( $1 ))
    if (( $LEVEL_CODE >= $( bb-log-level-code ) ))
    then
        local MESSAGE="$2"
        local PREFIX="$( bb-log-prefix )"
        local TIME="$( eval "$BB_LOG_TIME" )"
        local LEVEL="${BB_LOG_LEVEL_NAME[$LEVEL_CODE]}"
        local COLOR="${BB_LOG_COLORS[$LEVEL_CODE]}"
        local NOCOLOR="${BB_LOG_COLORS['NC']}"
        eval "echo -e $BB_LOG_FORMAT" >&2
    fi
}

bb-log-debug() {
    bb-log-msg $BB_LOG_DEBUG "$*"
}

bb-log-info() {
    bb-log-msg $BB_LOG_INFO "$*"
}

bb-log-warning() {
    bb-log-msg $BB_LOG_WARNING "$*"
}

bb-log-error() {
    bb-log-msg $BB_LOG_ERROR "$*"
}

bb-log-deprecated() {
    local ALTERNATIVE="$1"
    local CURRENT="${2-${FUNCNAME[1]}}"
    bb-log-warning "'$CURRENT' is deprecated, use '$ALTERNATIVE' instead"
}

bb-log-callstack() {
    local FRAME=$(( ${1-"1"} ))
    local MSG="Call stack is:"
    for (( i = $FRAME; i < ${#FUNCNAME[@]}; i++ ))
    do
        MSG="$MSG\n\t${BASH_SOURCE[$i]}:${BASH_LINENO[$i-1]}\t${FUNCNAME[$i]}()"
    done
    bb-log-debug "$MSG"
}

##
# ./source/03_exit.sh
#

BB_EXIT_CODE=0

bb-exit() {
    local CODE=$(( $1 ))
    local MSG="$2"
    bb-exit-helper $CODE "$MSG"
}

bb-exit-on-error() {
    if bb-error?
    then
        local MSG="$1"
        bb-exit-helper $BB_ERROR "$MSG"
    fi
}

bb-exit-helper() {
    local CODE=$(( $1 ))
    local MSG="$2"
    if (( $CODE == 0 ))
    then
        bb-log-info "$MSG"
    else
        bb-log-error "$MSG"
        bb-log-callstack 3
    fi
    BB_EXIT_CODE=$CODE
    exit $CODE
}

##
# ./source/04_assert.sh
#

bb-assert() {
    # Local vars are prefixed to avoid conflicts with ASSERTION expression
    local __ASSERTION="$1"
    local __MESSAGE="${2-Assertion error '$__ASSERTION'}"

    if ! eval "$__ASSERTION"
    then
        bb-exit $BB_ERROR_ASSERT_FAILED "$__MESSAGE"
    fi
}

bb-assert-root() {
    local __MESSAGE="${1-This script must be run as root!}"
    bb-assert '[[ $EUID -eq 0 ]]' "$__MESSAGE"
}

bb-assert-file() {
    local __FILE="$1"
    local __MESSAGE="${2-File '$__FILE' not found}"
    bb-assert '[[ -f $__FILE ]]' "$__MESSAGE"
}

bb-assert-file-readable() {
    local __FILE="$1"
    local __MESSAGE="${2-File '$__FILE' is not readable}"
    bb-assert '[[ -f $__FILE ]] && [[ -r $__FILE ]]' "$__MESSAGE"
}

bb-assert-file-writeable() {
    local __FILE="$1"
    local __MESSAGE="${2-File '$__FILE' is not writeable}"
    bb-assert '[[ -f $__FILE ]] && [[ -w $__FILE ]]' "$__MESSAGE"
}

bb-assert-file-executable() {
    local __FILE="$1"
    local __MESSAGE="${2-File '$__FILE' is not executable}"
    bb-assert '[[ -f $__FILE ]] && [[ -x $__FILE ]]' "$__MESSAGE"
}

bb-assert-dir() {
    local __DIR="$1"
    local __MESSAGE="${2-Directory '$__DIR' not found}"
    bb-assert '[[ -d $__DIR ]]' "$__MESSAGE"
}

bb-assert-var() {
    local __VAR="$1"
    local __MESSAGE="${2-Variable '$__VAR' not set}"
    bb-assert '[[ -n ${!__VAR} ]]' "$__MESSAGE"
}


##
# ./source/05_ext.sh
#

declare -A BB_EXT_BODIES

bb-ext-python() {
    local NAME="$1"
    BB_EXT_BODIES["$NAME"]="$( cat )"

    eval "$NAME() { python -c \"\${BB_EXT_BODIES[$NAME]}\" \"\$@\"; }"
}

# Additional parameters to be passed during invokation of Augeas.
bb-var BB_AUGEAS_PARAMS ""

# Root directory for Augeas.
bb-var BB_AUGEAS_ROOT "/"

bb-ext-augeas() {
    local NAME="$1"
    BB_EXT_BODIES["$NAME"]="$( cat )"

    eval "$NAME() {
    eval \"augtool -r \\\"\$BB_AUGEAS_ROOT\\\" \$BB_AUGEAS_PARAMS\" <<EOF
\${BB_EXT_BODIES[$NAME]}
EOF
}"
}


##
# ./source/09_exe.sh
#

bb-exe?() {
    local EXE="$1"
    type -t "$EXE" > /dev/null
}

##
# ./source/10_workspace.sh
#

bb-var BB_WORKSPACE ".bb-workspace"

bb-workspace-init() {
    bb-log-debug "Initializing workspace at '$BB_WORKSPACE'"
    if [[ ! -d "$BB_WORKSPACE" ]]
    then
        mkdir -p "$BB_WORKSPACE" || bb-exit \
            $BB_ERROR_WORKSPACE_CREATION_FAILED \
            "Failed to initialize workspace at '$BB_WORKSPACE'"
    fi
    # Ensure BB_WORKSPACE stores absolute path
    BB_WORKSPACE="$( readlink -ne $BB_WORKSPACE )"
}

bb-workspace-cleanup() {
    bb-log-debug "Cleaning up workspace at '$BB_WORKSPACE'"
    if [[ -z "$( ls "$BB_WORKSPACE" )" ]]
    then
        bb-log-debug "Workspace is empty. Removing"
        rm -rf "$BB_WORKSPACE"
    else
        bb-log-debug "Workspace is not empty"
    fi
}

##
# ./source/11_tmp.sh
#

bb-tmp-init() {
    BB_TMP_DIR="$BB_WORKSPACE/tmp_$( bb-tmp-name )"
    mkdir "$BB_TMP_DIR"
}

bb-tmp-file() {
    local FILENAME="$BB_TMP_DIR/$( bb-tmp-name )"
    touch "$FILENAME"
    echo "$FILENAME"
}

bb-tmp-dir() {
    local DIRNAME="$BB_TMP_DIR/$( bb-tmp-name )"
    mkdir -p "$DIRNAME"
    echo "$DIRNAME"
}

bb-tmp-name() {
    echo "$( date +%s )$RANDOM"
}

bb-tmp-cleanup() {
    rm -rf "$BB_TMP_DIR"
}


##
# ./source/12_template.sh
#

bb-template() {
    local TEMPLATE="$1"
    eval "echo \"$( < "$TEMPLATE" )\""
}

##
# ./source/13_properties.sh
#

bb-properties-read() {
    bb-log-deprecated 'bb-read-properties'
    bb-read-properties "$@"
}

##
# ./source/20_event.sh
#

declare -A BB_EVENT_DEPTH
BB_EVENT_MAX_DEPTH=1000

bb-event-init() {
    BB_EVENT_DIR="$( bb-tmp-dir )"
}

bb-event-on() {
    local EVENT="$1"
    local HANDLER="$2"
    local HANDLERS="$BB_EVENT_DIR/$EVENT.handlers"
    touch "$HANDLERS"
    if [[ -z "$( cat "$HANDLERS" | grep "^$HANDLER\$" )" ]]
    then
        bb-log-debug "Subscribed handler '$HANDLER' on event '$EVENT'"
        echo "$HANDLER" >> "$HANDLERS"
    fi
}

bb-event-off() {
    local EVENT="$1"
    local HANDLER="$2"
    local HANDLERS="$BB_EVENT_DIR/$EVENT.handlers"
    if [[ -f "$HANDLERS" ]]
    then
        bb-log-debug "Removed handler '$HANDLER' from event '$EVENT'"
        cat "$HANDLERS" | grep -v "^$HANDLER\$" > "$HANDLERS"
    fi
}

bb-event-fire() {
    [[ -n "$@" ]] || return 0

    local EVENT="$1"
    shift

    BB_EVENT_DEPTH["$EVENT"]=$(( ${BB_EVENT_DEPTH["$EVENT"]} + 1 ))
    if (( ${BB_EVENT_DEPTH["$EVENT"]} >= $BB_EVENT_MAX_DEPTH ))
    then
        bb-exit \
            $BB_ERROR_EVENT_MAX_DEPTH_REACHED \
            "Max recursion depth has been reached on processing event '$EVENT'"
    fi
    if [[ -f "$BB_EVENT_DIR/$EVENT.handlers" ]]
    then
        bb-log-debug "Run handlers for event '$EVENT'"
        while read -r HANDLER
        do
            eval "$HANDLER $@"
        done < "$BB_EVENT_DIR/$EVENT.handlers"
    fi
    BB_EVENT_DEPTH["$EVENT"]=$(( ${BB_EVENT_DEPTH["$EVENT"]} - 1 ))
}

bb-event-delay() {
    [[ -n "$@" ]] || return 0

    local EVENTS="$BB_EVENT_DIR/events"
    local EVENT=''

    while (( $# ))
    do
        EVENT+="$( printf "%q " "$1" )"
        shift
    done

    touch "$EVENTS"
    if [[ -z "$( cat "$EVENTS" | grep -Fx "$EVENT" )" ]]
    then
        bb-log-debug "Delayed event '$EVENT'"
        printf "%s\n" "$EVENT" >> "$EVENTS"
    fi
}

bb-event-cleanup() {
    BB_EVENT_DEPTH["__delay__"]=$(( ${BB_EVENT_DEPTH["__delay__"]} + 1 ))
    local EVENTS="$BB_EVENT_DIR/events"
    if (( ${BB_EVENT_DEPTH["__delay__"]} >= $BB_EVENT_MAX_DEPTH ))
    then
        bb-error "Max recursion depth has been reached on processing event '__delay__'"
        rm "$EVENTS"
        return $BB_ERROR_EVENT_MAX_DEPTH_REACHED
    fi
    if [[ -f "$EVENTS" ]]
    then
        local EVENT_LIST="$( bb-tmp-file )"
        cp -f "$EVENTS" "$EVENT_LIST"
        rm "$EVENTS"
        while read -r EVENT
        do
            bb-event-fire $EVENT
        done < "$EVENT_LIST"
        # If any event hadler calls "bb-event-delay", the "$EVENTS" file
        # will be created again and we should repeat this processing
        if [[ -f "$EVENTS" ]]
        then
            bb-event-cleanup
            if bb-error?
            then
                return $BB_ERROR
            fi
        fi
    fi
    BB_EVENT_DEPTH["__delay__"]=$(( ${BB_EVENT_DEPTH["__delay__"]} - 1 ))
}

##
# ./source/30_augeas.sh
#


# This variable can be used to provide additional commands to run by Augeas.
bb-var BB_AUGEAS_EXTRA_COMMANDS ""

bb-augeas?() {
    bb-exe? augtool
}

bb-augeas-get-path() {
    local ABSOLUTE_FILE_PATH="$1"
    local SETTING="$2"

    echo "/files$ABSOLUTE_FILE_PATH/$SETTING"
}

bb-augeas-file-supported?() {
    local ABSOLUTE_FILE_PATH="$1"
    local OUTPUT=

    # Define the helper function
    bb-ext-augeas 'bb-augeas-file-supported?-helper' <<EOF
$BB_AUGEAS_EXTRA_COMMANDS
match /augeas/load/*/incl '$ABSOLUTE_FILE_PATH'
print '/augeas/files$ABSOLUTE_FILE_PATH[count(error) = 0]/*'
EOF

    # Run the helper function
    OUTPUT="$(bb-augeas-file-supported?-helper)"
    bb-error? && bb-assert false "Failed to execute augeas"

    # File is supported if output is not empty.
    [[ -n "$OUTPUT" ]]
}

bb-augeas-get() {
    local ABSOLUTE_FILE_PATH="$1"
    local SETTING="$2"
    local AUG_PATH="$(bb-augeas-get-path "$ABSOLUTE_FILE_PATH" "$SETTING")"
    local VALUE=

    # Validate the specified file
    bb-augeas-file-supported? "$ABSOLUTE_FILE_PATH" || { bb-log-error "Cannot get value from unsupported file '$ABSOLUTE_FILE_PATH'"; return 1; }

    # Define the helper function
    bb-ext-augeas 'bb-augeas-get-helper' <<EOF
$BB_AUGEAS_EXTRA_COMMANDS
get '$AUG_PATH'
EOF

    # Run the helper function
    VALUE="$(bb-augeas-get-helper)"
    if bb-error?
    then
        bb-log-error "An error occured while getting value of '$SETTING' from $ABSOLUTE_FILE_PATH"
        return $BB_ERROR
    fi

    # Handle the result
    if [[ $VALUE == *" = "* ]]
    then
         # Value of the setting has been found
         # Output is in the form /files/.../<Setting> = Value
         VALUE="${VALUE#*=}"
         VALUE="${VALUE// }" # Remove leading spaces
         VALUE="${VALUE%%}"  # Remove trailing spaces
    elif [[ $VALUE == *" (none)"* ]]
    then
        # Setting has empty value
        VALUE=""
    else
        # Setting not found/set
        VALUE=""
    fi

    echo "$VALUE"
}

bb-augeas-match?() {
    local ABSOLUTE_FILE_PATH="$1"
    local SETTING="$2"
    local VALUE="$3"
    local AUG_PATH="$(bb-augeas-get-path "$ABSOLUTE_FILE_PATH" "$SETTING")"
    local OUTPUT

    # Validate the specified file
    bb-augeas-file-supported? "$ABSOLUTE_FILE_PATH" || { bb-log-error "Cannot match value from unsupported file '$ABSOLUTE_FILE_PATH'"; return 1; }

    # Define the helper function
    bb-ext-augeas 'bb-augeas-match-helper' <<EOF
$BB_AUGEAS_EXTRA_COMMANDS
match '$AUG_PATH' "$VALUE"
EOF

    # Run the helper function
    OUTPUT="$(bb-augeas-match-helper)"
    if bb-error?
    then
        bb-log-error "An error occured while verifying if '$SETTING' matches '$VALUE' ($AUG_PATH)"
        return $BB_ERROR
    fi

    # Check output
    # When there is a match, the output is in the form:
    #     /files/<File path>
    [[ "$OUTPUT" == "/files/"* ]]
}

bb-augeas-set() {
    local ABSOLUTE_FILE_PATH="$1"
    local SETTING="$2"
    local VALUE="$3"
    local AUG_PATH="$(bb-augeas-get-path "$ABSOLUTE_FILE_PATH" "$SETTING")"
    local OUTPUT=
    shift 3

    # Validate the specified file
    bb-augeas-file-supported? "$ABSOLUTE_FILE_PATH" || { bb-log-error "Cannot set value to unsupported file '$ABSOLUTE_FILE_PATH'"; return 1; }

    # Define the helper function
    bb-ext-augeas 'bb-augeas-set-helper' <<EOF
$BB_AUGEAS_EXTRA_COMMANDS
set "$AUG_PATH" "$VALUE"
save
EOF

    # Run the helper function
    OUTPUT="$(bb-augeas-set-helper)"
    if bb-error?
    then
        bb-log-error "An error occured while setting value of '$SETTING' to '$VALUE' ($ABSOLUTE_FILE_PATH)"
        return $BB_ERROR
    fi

    # Raise events if file changed
    if [[ "$OUTPUT" == "Saved "* ]]
    then
        bb-event-delay "$@"
        bb-event-fire "bb-augeas-file-changed" "$ABSOLUTE_FILE_PATH"
    fi
}


##
# ./source/30_download.sh
#

bb-var BB_DOWNLOAD_WGET_OPTIONS '-nv'

bb-download-init() {
    BB_DOWNLOAD_DIR="$BB_WORKSPACE/download"
}

bb-download() {
    if [[ ! -d "$BB_DOWNLOAD_DIR" ]]
    then
        bb-log-debug "Creating download directory at '$BB_DOWNLOAD_DIR'"
        mkdir "$BB_DOWNLOAD_DIR"
    fi

    local URL="$1"
    local TARGET="${2-$( basename "$URL" )}"
    local FORCE="${3-false}"
    TARGET="$BB_DOWNLOAD_DIR/$TARGET"
    echo "$TARGET"
    if [[ -f "$TARGET" ]] && ! $FORCE
    then
        return 0
    fi

    bb-log-info "Downloading $URL"
    wget $BB_DOWNLOAD_WGET_OPTIONS -O "$TARGET" "$URL"
    if bb-error?
    then
        bb-log-error "An error occurs while downloading $URL"
        return $BB_ERROR
    fi
}

bb-download-clean() {
    rm -rf "$BB_DOWNLOAD_DIR"
}

##
# ./source/30_flag.sh
#

bb-flag-init() {
    BB_FLAG_DIR="$BB_WORKSPACE/flag"
}

bb-flag?() {
    local FLAG="$1"
    [[ -f "$BB_FLAG_DIR/$FLAG" ]]
}

bb-flag-set() {
    local FLAG="$1"
    if [[ ! -d "$BB_FLAG_DIR" ]]
    then
        bb-log-debug "Creating flag directory at '$BB_DOWNLOAD_DIR'"
        mkdir "$BB_FLAG_DIR"
    fi
    touch "$BB_FLAG_DIR/$FLAG"
}

bb-flag-unset() {
    local FLAG="$1"
    [[ ! -f "$BB_FLAG_DIR/$FLAG" ]] || rm "$BB_FLAG_DIR/$FLAG"
}

bb-flag-clean() {
    bb-log-debug "Removing flag directory"
    rm -rf "$BB_FLAG_DIR"
}

bb-flag-cleanup() {
    if [[ -d "$BB_FLAG_DIR" && -z "$( ls "$BB_FLAG_DIR" )" ]]
    then
        bb-log-debug "Flag directory is empty"
        bb-flag-clean
    fi
}

##
# ./source/30_read.sh
#

# normalizing, reading and evaluating key=value lines from the properties file
# regexp searches for lines with key=value, key:value, key: value etc.. pattern,
# see http://docs.oracle.com/javase/7/docs/api/java/util/Properties.html#load(java.io.Reader)
bb-ext-python 'bb-read-properties-helper' <<EOF
import re
import sys

filename = sys.argv[1]
prefix = sys.argv[2]
with open(filename, 'r') as properties:
    for line in properties:
        line = line.strip()
        match = re.match(r'^(?P<key>[^#!]*?)[\s:=]+(?P<value>.+)', line)
        if match:
            match = match.groupdict()
            match['key'] = re.sub(r'[\W]', '_', match['key'])
            print('{prefix}{key}="{value}"'.format(prefix=prefix, **match))
EOF

bb-read-properties() {
    local FILENAME="$1"
    local PREFIX="$2"

    if [[ ! -r "$FILENAME" ]]
    then
        bb-log-error "'$FILENAME' is not readable"
        return 1
    fi

    eval "$( bb-read-properties-helper "$FILENAME" "$PREFIX" )"
}


bb-ext-python 'bb-read-ini-helper' <<EOF
import re
import sys
try:
    from ConfigParser import SafeConfigParser as ConfigParser
except ImportError:
    # Python 3.x
    from configparser import ConfigParser

filename = sys.argv[1]
section = sys.argv[2]
prefix = sys.argv[3]
reader = ConfigParser()
reader.read(filename)

if not section or section == '*':
    sections = reader.sections()
else:
    sections = [section]
for section in sections:
    for key, value in reader.items(section):
        section = re.sub(r'[\W]', '_', section)
        key = re.sub(r'[\W]', '_', key)
        print(
            '{prefix}{section}_{key}="{value}"'.format(
                prefix=prefix,
                section=section,
                key=key,
                value=value
            )
        )
EOF

bb-read-ini() {
    local FILENAME="$1"
    local SECTION="$2"
    local PREFIX="$3"

    if [[ ! -r "$FILENAME" ]]
    then
        bb-log-error "'$FILENAME' is not readable"
        return 1
    fi

    eval "$( bb-read-ini-helper "$FILENAME" "$SECTION" "$PREFIX" )"
}


bb-ext-python 'bb-read-json-helper' <<EOF
import re
import sys
import json

filename = sys.argv[1]
prefix = sys.argv[2]

def serialize(value, name):
    if value is None:
        print('{0}=""'.format(name))
    elif hasattr(value, 'items'):
        for key, subvalue in value.items():
            key = re.sub(r'[\W]', '_', key)
            serialize(subvalue, name + '_' + key)
    elif hasattr(value, '__iter__'):
        print("{0}_len={1}".format(name, len(value)))
        for i, v in enumerate(value):
            serialize(v, name + '_' + str(i))
    else:
        print('{0}="{1}"'.format(name, value))

with open(filename, 'r') as json_file:
    data = json.load(json_file)
    serialize(data, prefix)

EOF

bb-read-json() {
    local FILENAME="$1"
    local PREFIX="$2"

    if [[ ! -r "$FILENAME" ]]
    then
        bb-log-error "'$FILENAME' is not readable"
        return 1
    fi

    eval "$( bb-read-json-helper "$FILENAME" "$PREFIX" )"
}


bb-ext-python 'bb-read-yaml-helper' <<EOF
import re
import sys
import yaml

filename = sys.argv[1]
prefix = sys.argv[2]

def serialize(value, name):
    if value is None:
        print('{0}=""'.format(name))
    elif hasattr(value, 'items'):
        for key, subvalue in value.items():
            key = re.sub(r'[\W]', '_', key)
            serialize(subvalue, name + '_' + key)
    elif hasattr(value, '__iter__'):
        print("{0}_len={1}".format(name, len(value)))
        for i, v in enumerate(value):
            serialize(v, name + '_' + str(i))
    else:
        print('{0}="{1}"'.format(name, value))

with open(filename, 'r') as yaml_file:
    data = yaml.load(yaml_file)
    serialize(data, prefix)

EOF

bb-ext-python 'bb-read-yaml?' <<EOF
try:
    import yaml
except ImportError:
    exit(1)

EOF

bb-read-yaml() {
    local FILENAME="$1"
    local PREFIX="$2"

    if [[ ! -r "$FILENAME" ]]
    then
        bb-log-error "'$FILENAME' is not readable"
        return 1
    fi

    eval "$( bb-read-yaml-helper "$FILENAME" "$PREFIX" )"
}



##
# ./source/30_sync.sh
#

bb-sync-file() {
    local DST_FILE="$1"
    local SRC_FILE="$2"
    shift 2

    local DST_FILE_CHANGED=false

    if [[ ! -f "$DST_FILE" ]]
    then
        touch "$DST_FILE"
        DST_FILE_CHANGED=true
    fi
    if [[ -n "$( diff -q "$SRC_FILE" "$DST_FILE" )" ]]
    then
        cp -f -p "$SRC_FILE" "$DST_FILE"
        DST_FILE_CHANGED=true
    fi

    if $DST_FILE_CHANGED
    then
        bb-event-fire "bb-sync-file-changed" "$DST_FILE"
        bb-event-delay "$@"
    fi
}

bb-sync-dir() {
    local TWO_WAY=true
    local -a ARGS=()
    local -i TEST=3    # Test first three arguments

    while (( $# && TEST-- ))
    do
        case "$1" in
            -o|--one-way) TWO_WAY=false ;;
            -t|--two-way) TWO_WAY=true  ;;
            *) ARGS[${#ARGS[@]}]="$1"   ;;
        esac
        shift
    done
    ARGS=( "${ARGS[@]}" "$@" )

    bb-sync-dir-helper "$TWO_WAY" "${ARGS[@]}"
}

bb-sync-dir-helper() {
    local TWO_WAY="$1"
    local DST_DIR="$( readlink -nm "$2" )"
    local SRC_DIR="$( readlink -ne "$3" )"
    shift 3

    if [[ ! -d "$DST_DIR" ]]
    then
        mkdir -p "$DST_DIR"
        bb-event-delay "$@"
        bb-event-fire "bb-sync-dir-created" "$DST_DIR"
    fi

    local ORIGINAL_DIR="$( pwd )"
    local NAME

    cd "$SRC_DIR"
    while read -r NAME
    do
        if [[ -f "$SRC_DIR/$NAME" ]]
        then
            bb-sync-file "$DST_DIR/$NAME" "$SRC_DIR/$NAME" "$@"
        elif [[ -d "$SRC_DIR/$NAME" ]]
        then
            bb-sync-dir-helper "$TWO_WAY" "$DST_DIR/$NAME" "$SRC_DIR/$NAME" "$@"
        fi
    done < <( ls -A )

    if $TWO_WAY
    then
        cd "$DST_DIR"
        while read -r NAME
        do
            if [[ ! -e "$SRC_DIR/$NAME" ]]
            then
                local EVENT="bb-sync-file-removed"
                if [[ -d "$DST_DIR/$NAME" ]]
                then
                    EVENT="bb-sync-dir-removed"
                fi
                rm -rf "$DST_DIR/$NAME"
                bb-event-delay "$@"
                bb-event-fire "$EVENT" "$DST_DIR/$NAME"
            fi
        done < <( find . )
    fi

    cd "$ORIGINAL_DIR"
}

##
# ./source/30_task.sh
#

declare -A BB_TASK_FUNCS
declare -a BB_TASK_CONTEXT

bb-task-def() {
    local NAME="$1"
    local FUNC="${2-${NAME}}"
    BB_TASK_FUNCS[$NAME]="$FUNC"
}

bb-task-run() {
    BB_TASK_CONTEXT[${#BB_TASK_CONTEXT[@]}]="$( bb-tmp-file )"
    bb-task-depends "$@"
    unset BB_TASK_CONTEXT[${#BB_TASK_CONTEXT[@]}-1]
}

bb-task-depends() {
    local CONTEXT="${BB_TASK_CONTEXT[${#BB_TASK_CONTEXT[@]}-1]}"
    local CODE
    local NAME
    local TASK

    if [[ ! -f "$CONTEXT" ]]
    then
        bb-exit $BB_ERROR_TASK_BAD_CONTEXT "Cannot run tasks. Bad context"
    fi
    for NAME in "$@"
    do
        if [[ -z $( cat "$CONTEXT" | grep "^$NAME$" ) ]]
        then
            bb-log-info "Running task '$NAME'..."
            TASK=${BB_TASK_FUNCS[$NAME]}
            if [[ -z "$TASK" ]]
            then
                bb-exit $BB_ERROR_TASK_UNDEFINED "Undefined task '$NAME'"
            fi
            $TASK
            CODE=$?
            if (( $CODE != 0 ))
            then
                bb-exit $CODE "Task '$NAME' failed"
            fi
            bb-log-info "Task '$NAME' OK"
        fi
        echo "$NAME" >> "$CONTEXT"
    done
}

##
# ./source/30_wait.sh
#

bb-wait() {
    local __CONDITION="$1"
    local __TIMEOUT="$2"
    local __COUNTER=$(( $__TIMEOUT ))

    while ! eval "$__CONDITION"
    do
        sleep 1
        if [[ -n "$__TIMEOUT" ]]
        then
            __COUNTER=$(( $__COUNTER - 1 ))
            if (( $__COUNTER <= 0 ))
            then
                bb-log-error "Timeout has been reached during wait for '$__CONDITION'"
                return 1
            fi
        fi
    done
}

##
# ./source/40_iptables.sh
#

# True if `iptables` is executable
bb-iptables?() {
    bb-exe? iptables
}

# True if given chain exists.
bb-iptables-chain?() {
    local USAGE='bb-iptables-chain? [-t,--table TABLE=filter] CHAIN'
    local TABLE="filter"
    local ARG
    local -a ARGS
    while [[ $# > 0 ]]
    do
        KEY="$1"
        shift
        case $KEY in
            --table=*)
                TABLE="${KEY#*=}"
                ;;
            -t|--table)
                TABLE="$1"
                shift
                ;;
            *)
                ARGS+=("$KEY")
                ;;
        esac
    done
    local CHAIN="${ARGS[0]}"
    bb-assert 'test -n "$CHAIN"' "usage: $USAGE"
    iptables -t $TABLE -nL "$CHAIN" > /dev/null 2> /dev/null
}

# Create chain if does not already exist.
bb-iptables-chain() {
    local USAGE='bb-iptables-chain [-t,--table TABLE=filter] CHAIN'
    local TABLE="filter"
    local ARG
    local -a ARGS
    while [[ $# > 0 ]]
    do
        KEY="$1"
        shift
        case $KEY in
            --table=*)
                TABLE="${KEY#*=}"
                ;;
            -t|--table)
                TABLE="$1"
                shift
                ;;
            *)
                ARGS+=("$KEY")
                ;;
        esac
    done
    local CHAIN="${ARGS[0]}"
    bb-assert 'test -n "$CHAIN"' "usage: $USAGE"
    if ! bb-iptables-chain? -t "$TABLE" "$CHAIN"
    then
        bb-log-info "creating chain $CHAIN in table $TABLE"
        iptables -t $TABLE -N "$CHAIN"
    fi
}

# Return rule number for rule with matching ID.
# If no rule matches, return "".
# Exit with error if multiple rules match given ID.
bb-iptables-rule-num() {
    local USAGE='bb-iptables-rule-num [-t,--table TABLE=filter] CHAIN ID'
    local TABLE="filter"
    local ARG
    local -a ARGS
    while [[ $# > 0 ]]
    do
        KEY="$1"
        shift
        case $KEY in
            --table=*)
                TABLE="${KEY#*=}"
                ;;
            -t|--table)
                TABLE="$1"
                shift
                ;;
            *)
                ARGS+=("$KEY")
                ;;
        esac
    done
    local CHAIN="${ARGS[0]}"
    local ID="${ARGS[1]}"
    bb-assert 'test -n "$CHAIN" -a -n "$ID"' "usage: $USAGE"
    NUMS=$(iptables -t $TABLE -nL "$CHAIN" --line-numbers | awk -v ID="$ID" '$0 ~ "/[*] " ID " [*]/" { print $1 }')
    bb-assert 'test $(echo $NUMS | wc -w) -lt 2' "rule '$ID' matches multiple rules: $NUMS"
    echo $NUMS
}

# Return checksum for matching rule.
bb-iptables-rule-hash() {
    local USAGE='bb-iptables-rule-hash [-t,--table TABLE=filter] CHAIN NUM'
    local TABLE="filter"
    local ARG
    local -a ARGS
    while [[ $# > 0 ]]
    do
        KEY="$1"
        shift
        case $KEY in
            --table=*)
                TABLE="${KEY#*=}"
                ;;
            -t|--table)
                TABLE="$1"
                shift
                ;;
            *)
                ARGS+=("$KEY")
                ;;
        esac
    done
    local CHAIN="${ARGS[0]}"
    local NUM="${ARGS[1]}"
    bb-assert 'test -n "$CHAIN" -a -n "$ID"' "usage: $USAGE"
    iptables -n -t $TABLE -L "$CHAIN" $NUM | md5sum | awk '{print $1}'
}

# Define rule in CHAIN and position NUM.
# If rule with matching ID exists, then update it.
# When NUM is negative, count from end of CHAIN (-1 == last rule).
bb-iptables-rule() {
    local USAGE='bb-iptables-rule [-t,--table TABLE=filter] [-n,--num NUM=-1] CHAIN ID IPTABLES_RULE...'
    local TABLE="filter"
    local NUM="-1"
    local ARG
    local -a ARGS
    while [[ $# > 0 ]]
    do
        KEY="$1"
        shift
        case $KEY in
            --table=*)
                TABLE="${KEY#*=}"
                ;;
            -t|--table)
                TABLE="$1"
                shift
                ;;
            --num=*)
                NUM="${KEY#*=}"
                ;;
            -n|--num)
                NUM="$1"
                shift
                ;;
            *)
                ARGS+=("$KEY")
                ;;
      esac
    done
    local CHAIN="${ARGS[0]}"
    local ID="${ARGS[1]}"
    unset ARGS[0] ARGS[1]
    bb-assert 'test -n "$CHAIN" -a -n "$ID"' "usage: $USAGE"
    local DEF="${ARGS[@]}"
    local RULE=$(bb-iptables-rule-num -t "$TABLE" "$CHAIN" "$ID")
    if [[ -n "$RULE" ]]
    then
        local RULEHASH=$(bb-iptables-rule-hash -t "$TABLE" "$CHAIN" $RULE)
        iptables -t $TABLE -R "$CHAIN" $RULE $DEF -m comment --comment "$ID"
        if [[ "$RULEHASH" != $(bb-iptables-rule-hash -t "$TABLE" "$CHAIN" $RULE) ]]
        then
            bb-log-info "replaced rule: $ID at $RULE"
        fi
    else
        TOTAL=$(( $(iptables -t $TABLE -nL "$CHAIN" | wc -l) - 2 ))
        if [[ $NUM -eq -1 ]]
        then
            # Handle append case specially.
            OP="-A"
            NUM=""
        else
            OP="-I"
            if [[ $NUM -lt 0 ]]
            then
                # Count from the end of the chain.
                NUM=$(( $TOTAL + $NUM + 2 ))
            fi
            if [[ $NUM -lt 1 ]]
            then
                # When there are no rules in chain or user specifies 0.
                NUM=1
            fi
        fi
        if [[ -n "$BB_DRY_RUN" ]]
        then
            echo "iptables -t $TABLE $OP '$CHAIN' $NUM $DEF -m comment --comment '$ID'"
        else
            bb-log-info "defining new rule: $ID at ${NUM:-END}"
            iptables -t $TABLE $OP "$CHAIN" $NUM $DEF -m comment --comment "$ID"
        fi
    fi
}

##
# ./source/50_apt.sh
#

bb-var BB_APT_UPDATED false

bb-apt?() {
    bb-exe? apt-get
}

bb-apt-repo?() {
    local REPO=$1
    cat /etc/apt/sources.list /etc/apt/sources.list.d/* 2> /dev/null | grep -v '^#' | grep -qw "$REPO"
}

bb-apt-package?() {
    local PACKAGE=$1
    dpkg -s "$PACKAGE" 2> /dev/null | grep -q '^Status:.\+installed'
}

bb-apt-update() {
    $BB_APT_UPDATED && return 0
    bb-log-info 'Updating apt cache'
    apt-get update
    BB_APT_UPDATED=true
}

bb-apt-install() {
    for PACKAGE in "$@"
    do
        if ! bb-apt-package? "$PACKAGE"
        then
            bb-apt-update
            bb-log-info "Installing package '$PACKAGE'"
            apt-get install -y "$PACKAGE"
            bb-exit-on-error "Failed to install package '$PACKAGE'"
            bb-event-fire "bb-package-installed" "$PACKAGE"
        fi
    done
}

bb-apt-package-upgrade?() {
    bb-apt-update

    local PACKAGE=$1
    local OUTPUT="$(
        apt-cache policy "$PACKAGE" |
        grep -A 1 'Installed: ' |
        sed -r 's/(Installed: |Candidate: )//' |
        sed '/ (none)/I,+1 d' |
        uniq -u
    )"

    # Note: No upgrade available is reported for a non-installed package
    [[ -n "$OUTPUT" ]]
}

bb-apt-upgrade() {
    for PACKAGE in "$@"
    do
        if bb-apt-package-upgrade? "$PACKAGE"
        then
            bb-log-info "Upgrading package '$PACKAGE'"
            bb-event-fire "bb-package-pre-upgrade" "$PACKAGE"
            apt-get upgrade -y "$PACKAGE"
            bb-exit-on-error "Failed to upgrade package '$PACKAGE'"
            bb-event-fire "bb-package-post-upgrade" "$PACKAGE"
        fi
    done
}


##
# ./source/50_brew.sh
#

bb-var BB_BREW_UPDATED false

bb-brew?() {
    bb-exe? brew
}

bb-brew-repo?() {
    local REPO=$1
    brew tap | grep -q "$REPO"
}

bb-brew-package?() {
    local PACKAGE=$1
    [ -n "$( brew ls --versions "$PACKAGE" )" ]
}

bb-brew-update() {
    $BB_BREW_UPDATED && return 0
    bb-log-info 'Updating Homebrew'
    brew update
    BB_BREW_UPDATED=true
}

bb-brew-install() {
    for PACKAGE in "$@"
    do
        if ! bb-brew-package? "$PACKAGE"
        then
            bb-brew-update
            bb-log-info "Installing package '$PACKAGE'"
            brew install "$PACKAGE"
            bb-exit-on-error "Failed to install package '$PACKAGE'"
            bb-event-fire "bb-package-installed" "$PACKAGE"
        fi
    done
}

##
# ./source/50_yum.sh
#

bb-var BB_YUM_UPDATED false

bb-yum?() {
    bb-exe? yum
}

bb-yum-repo?() {
    local REPO=$1
    yum -C repolist | grep -Ewq "^(\W)*${REPO}"
}

bb-yum-package?() {
    local PACKAGE=$1
    yum -C list installed "$PACKAGE" &> /dev/null
}

bb-yum-update() {
    $BB_YUM_UPDATED && return 0
    bb-log-info 'Updating yum cache'
    yum clean all
    yum makecache
    BB_YUM_UPDATED=true
}

bb-yum-install() {
    for PACKAGE in "$@"
    do
        if ! bb-yum-package? "$PACKAGE"
        then
            bb-yum-update
            bb-log-info "Installing package '$PACKAGE'"
            yum install -y "$PACKAGE"
            bb-exit-on-error "Failed to install package '$PACKAGE'"
            bb-event-fire "bb-package-installed" "$PACKAGE"
        fi
    done
}

##
# ./source/99_init.sh
#

bb-workspace-init
bb-tmp-init
bb-event-init
bb-download-init
bb-flag-init


bb-cleanup-update-exit-code() {
    if bb-error? && (( $BB_EXIT_CODE == 0 ))
    then
        BB_EXIT_CODE=$BB_ERROR
    fi
}

bb-cleanup() {
    bb-cleanup-update-exit-code

    bb-event-fire bb-cleanup        ; bb-cleanup-update-exit-code

    bb-flag-cleanup                 ; bb-cleanup-update-exit-code
    bb-event-cleanup                ; bb-cleanup-update-exit-code
    bb-tmp-cleanup                  ; bb-cleanup-update-exit-code
    bb-workspace-cleanup            ; bb-cleanup-update-exit-code

    exit $BB_EXIT_CODE
}

trap bb-cleanup EXIT

