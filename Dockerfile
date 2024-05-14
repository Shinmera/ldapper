FROM debian:bookworm

RUN \
	apt-get update && \
	apt-get upgrade && \
	apt-get install -y sbcl curl gcc gpg && \
	apt-get clean && \
	cd /root && \
	curl -O https://beta.quicklisp.org/quicklisp.lisp && \
	curl -O https://beta.quicklisp.org/quicklisp.lisp.asc && \
	curl -O https://beta.quicklisp.org/release-key.txt && \
	gpg --import release-key.txt && \
	gpg --verify quicklisp.lisp.asc quicklisp.lisp && \
	rm -rf /root/quicklisp.lisp.asc /root/release-key.txt && \
	sbcl --load /root/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql-util:without-prompting (ql:add-to-init-file))'

WORKDIR /app

ENTRYPOINT ["/app/docker_build.sh"]
