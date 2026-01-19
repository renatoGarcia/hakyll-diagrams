docker-image := "renatogarcia/hakyll-diagrams-ci"
ci-cache := "./ci-files"

install-cabal:
    docker run --rm -v {{ci-cache}}/ghcup:/github/home/.ghcup -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e HOME=/github/home {{docker-image}} ghcup install cabal 3.16.1.0

install-ghc ghcver:
    docker run --rm -v {{ci-cache}}/ghcup:/github/home/.ghcup -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e HOME=/github/home {{docker-image}} ghcup install ghc {{ghcver}}
    docker run --rm -v {{ci-cache}}/ghcup:/github/home/.ghcup -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e HOME=/github/home {{docker-image}} ghcup set ghc {{ghcver}}

update-cabal: install-cabal
    docker run --rm -v {{ci-cache}}/ghcup:/github/home/.ghcup -v {{ci-cache}}/cabal:/github/home/cabal -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e CABAL_DIR=/github/home/cabal -e HOME=/github/home {{docker-image}} cabal update

build projfile ghcver: (install-ghc ghcver) update-cabal
    docker run --rm -v .:/host -v {{ci-cache}}/ghcup:/github/home/.ghcup -v {{ci-cache}}/cabal:/github/home/cabal -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e CABAL_DIR=/github/home/cabal -e HOME=/github/home -w /host {{docker-image}} cabal build --project-file={{projfile}} --builddir=dist-{{ghcver}}

test projfile:
    docker run --rm -v .:/host -v {{ci-cache}}/ghcup:/github/home/.ghcup -v {{ci-cache}}/cabal:/github/home/cabal -w /host -e GHCUP_INSTALL_BASE_PREFIX=/github/home/ -e CABAL_DIR=/github/home/cabal -e HOME=/github/home {{docker-image}} cabal test --project-file={{projfile}}

build-all:
    jq -c '.[]' .justfile.json | while read row; do \
      just build                                    \
        $(jq -r '.projfile' <<< "$row")             \
        $(jq -r '.ghcver' <<< "$row");              \
    done

test-all:
    jq -c '.[]' .justfile.json | while read row; do \
       just test $(jq -r '.projfile' <<< "$row");   \
    done
