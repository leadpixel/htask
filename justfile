default:
    @just --list

# run tests
test:
    go test ./...

# build htask
build:
    go build -o htask ./cmd/htask

# install htask
install:
    go install ./cmd/htask
