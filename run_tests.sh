#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./run_tests.sh              # runs lex on tests/01-lexing, parse on tests/02-parsing, typecheck on tests/03-typechecking
#   ./run_tests.sh lex          # runs only lex on tests/01-lexing
#   ./run_tests.sh parse        # runs only parse on tests/02-parsing
#   ./run_tests.sh typecheck    # runs only typecheck on tests/03-typechecking

MODE="${1:-all}"

LEX_DIR="tests/01-lexing"
PARSE_DIR="tests/02-parsing"
TC_DIR="tests/03-typechecking"

# Use opam exec if dune is not directly in PATH
if command -v dune >/dev/null 2>&1; then
  DUNE_CMD=(dune)
elif command -v opam >/dev/null 2>&1; then
  DUNE_CMD=(opam exec -- dune)
else
  echo "Error: neither 'dune' nor 'opam' found in PATH." >&2
  exit 1
fi

echo "==> Building project with '${DUNE_CMD[*]} build'..."
"${DUNE_CMD[@]}" build
echo "==> Build succeeded!"

RUN_LEX=false
RUN_PARSE=false
RUN_TC=false

case "$MODE" in
  all)
    RUN_LEX=true
    RUN_PARSE=true
    RUN_TC=true
    ;;
  lex|lexer|01|tests/01-lexing*)
    RUN_LEX=true
    ;;
  parse|parser|02|tests/02-parsing*)
    RUN_PARSE=true
    ;;
  tc|typecheck|typechecker|03|tests/03-typechecking*)
    RUN_TC=true
    ;;
  *)
    echo "Unknown mode '$MODE'. Use: all | lex | parse | typecheck" >&2
    exit 1
    ;;
esac

LEX_FILES=()
PARSE_FILES=()
TC_FILES=()

if $RUN_LEX && [ -d "$LEX_DIR" ]; then
  mapfile -t LEX_FILES < <(find "$LEX_DIR" -type f -name "*.cpe" | sort)
fi

if $RUN_PARSE && [ -d "$PARSE_DIR" ]; then
  mapfile -t PARSE_FILES < <(find "$PARSE_DIR" -type f -name "*.cpe" | sort)
fi

if $RUN_TC && [ -d "$TC_DIR" ]; then
  mapfile -t TC_FILES < <(find "$TC_DIR" -type f -name "*.cpe" | sort)
fi

# Build a single batch of utop commands so dune utop only starts once
UTOP_INPUT=""
for file in "${LEX_FILES[@]}"; do
  UTOP_INPUT+="CpecmuCompiler.A1_lexing.lex_test \"${file}\";;"$'\n'
done
for file in "${PARSE_FILES[@]}"; do
  UTOP_INPUT+="CpecmuCompiler.A2_parsing.parse_test \"${file}\";;"$'\n'
done
for file in "${TC_FILES[@]}"; do
  UTOP_INPUT+="CpecmuCompiler.A3_typechecking.typecheck_file \"${file}\";;"$'\n'
done

UTOP_OUTPUT=""
if [ -n "$UTOP_INPUT" ]; then
  UTOP_OUTPUT=$(printf "%s" "$UTOP_INPUT" | "${DUNE_CMD[@]}" utop src 2>&1)
fi

# ============================================================
# 1. LEXER RESULTS (tests/01-lexing)
# ============================================================
LEX_PASS=0
LEX_FAIL=0

if $RUN_LEX; then
  echo
  echo "============================================================"
  echo " [1] LEXING ($LEX_DIR) - ${#LEX_FILES[@]} file(s)"
  echo "============================================================"
  if [ "${#LEX_FILES[@]}" -eq 0 ]; then
    echo "No .cpe files found in $LEX_DIR."
  else
    for file in "${LEX_FILES[@]}"; do
      outfile="out/01-lexing/${file}"
      if [ -f "$outfile" ]; then
        tokens_inline=$(tr '\n' ' ' < "$outfile" | sed 's/ *$//')
        if grep -q "^Error(" "$outfile"; then
          err_line=$(grep "^Error(" "$outfile" | head -n 1)
          echo "[FAIL]   $file -> $err_line"
          LEX_FAIL=$((LEX_FAIL + 1))
        else
          echo "[PASS]   $file -> $tokens_inline"
          LEX_PASS=$((LEX_PASS + 1))
        fi
      else
        echo "[FAIL]   $file (no output generated)"
        LEX_FAIL=$((LEX_FAIL + 1))
      fi
    done
  fi
  echo "------------------------------------------------------------"
  echo "Lexing Summary:       Total=${#LEX_FILES[@]} | Passed=${LEX_PASS} | Failed=${LEX_FAIL}"
fi

# ============================================================
# 2. PARSER RESULTS (tests/02-parsing)
# ============================================================
PARSE_PASS=0
PARSE_FAIL=0

if $RUN_PARSE; then
  echo
  echo "============================================================"
  echo " [2] PARSING ($PARSE_DIR) - ${#PARSE_FILES[@]} file(s)"
  echo "============================================================"
  if [ "${#PARSE_FILES[@]}" -eq 0 ]; then
    echo "No .cpe files found in $PARSE_DIR."
  else
    for file in "${PARSE_FILES[@]}"; do
      outfile="out/02-parsing/${file}"
      if [ -f "$outfile" ]; then
        sexpr_inline=$(tr '\n' ' ' < "$outfile" | sed 's/  */ /g; s/^ *//; s/ *$//')
        if grep -q "^syntax error$" "$outfile"; then
          echo "[SYNTAX] $file -> syntax error"
          PARSE_FAIL=$((PARSE_FAIL + 1))
        else
          echo "[PASS]   $file -> $sexpr_inline"
          PARSE_PASS=$((PARSE_PASS + 1))
        fi
      else
        echo "[FAIL]   $file (no output generated)"
        PARSE_FAIL=$((PARSE_FAIL + 1))
      fi
    done
    # Show parser syntax error locations for tests/02-parsing
    while IFS= read -r line; do
      clean_line="${line#"# "}"
      while [[ "$clean_line" == "# "* ]]; do
        clean_line="${clean_line#"# "}"
      done
      if [[ "$clean_line" =~ ^${PARSE_DIR}/.*\.cpe:[0-9]+:[0-9]+ ]]; then
        echo "         -> $clean_line"
      fi
    done <<< "$UTOP_OUTPUT"
  fi
  echo "------------------------------------------------------------"
  echo "Parsing Summary:      Total=${#PARSE_FILES[@]} | Passed=${PARSE_PASS} | Syntax Error=${PARSE_FAIL}"
fi

# ============================================================
# 3. TYPECHECKER RESULTS (tests/03-typechecking)
# ============================================================
TC_PASS=0
TC_FAIL=0
TC_SYNTAX=0

if $RUN_TC; then
  echo
  echo "============================================================"
  echo " [3] TYPECHECKING ($TC_DIR) - ${#TC_FILES[@]} file(s)"
  echo "============================================================"
  if [ "${#TC_FILES[@]}" -eq 0 ]; then
    echo "No .cpe files found in $TC_DIR."
  else
    while IFS= read -r line; do
      clean_line="${line#"# "}"
      while [[ "$clean_line" == "# "* ]]; do
        clean_line="${clean_line#"# "}"
      done

      if [[ "$clean_line" == ${TC_DIR}/*": typechecking passed"* ]]; then
        echo "[PASS]   $clean_line"
        TC_PASS=$((TC_PASS + 1))
      elif [[ "$clean_line" == ${TC_DIR}/*": typechecking failed"* ]]; then
        echo "[FAIL]   $clean_line"
        TC_FAIL=$((TC_FAIL + 1))
      elif [[ "$clean_line" == ${TC_DIR}/*": syntax error"* ]]; then
        echo "[SYNTAX] $clean_line"
        TC_SYNTAX=$((TC_SYNTAX + 1))
      elif [[ "$clean_line" =~ ^${TC_DIR}/.*\.cpe:[0-9]+:[0-9]+ ]]; then
        echo "         -> $clean_line"
      fi
    done <<< "$UTOP_OUTPUT"
  fi
  echo "------------------------------------------------------------"
  echo "Typechecking Summary: Total=${#TC_FILES[@]} | Passed=${TC_PASS} | Failed=${TC_FAIL} | Syntax Error=${TC_SYNTAX}"
fi

echo
echo "============================================================"
echo " OVERALL SUMMARY"
echo "============================================================"
if $RUN_LEX; then
  echo "  01-lexing       (${#LEX_FILES[@]} files) : ${LEX_PASS} passed, ${LEX_FAIL} failed"
fi
if $RUN_PARSE; then
  echo "  02-parsing      (${#PARSE_FILES[@]} files) : ${PARSE_PASS} passed, ${PARSE_FAIL} syntax error(s)"
fi
if $RUN_TC; then
  echo "  03-typechecking (${#TC_FILES[@]} files) : ${TC_PASS} passed, ${TC_FAIL} failed, ${TC_SYNTAX} syntax error(s)"
fi
echo "============================================================"
