FROM public.ecr.aws/prima/node:14.18.1-1

WORKDIR /code

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt-get update && \
    apt-get install -qqy yarn && \
    apt-get clean && \
    chown -R node:node /code

# Serve per avere l'owner dei file scritti dal container uguale all'utente Linux sull'host
USER node

ENTRYPOINT ["/bin/bash"]
