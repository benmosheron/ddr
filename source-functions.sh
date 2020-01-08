
function build {
  scalac -cp . -d out ben/*.scala
}

function runOnly {
  scala -cp .:out ben.App "$@"
}

function run {
  if [ "$1" == "--build" ]; then
    echo "Building..."
    if build ; then
      runOnly "${@:2}"
    fi
  else
    runOnly "$@"
  fi
}

