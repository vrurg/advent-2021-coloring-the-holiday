FROM croservices/cro-http:0.8.7
RUN mkdir /app
COPY . /app
WORKDIR /app
RUN zef install --deps-only . && perl6 -c -Ilib service.p6
ENV FOO_PORT="10000" FOO_HOST="0.0.0.0"
EXPOSE 10000
CMD perl6 -Ilib service.p6
