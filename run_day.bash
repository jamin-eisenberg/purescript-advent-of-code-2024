day=$1
shift 1
npx spago@next run --main "Day${day}" -- "$@"