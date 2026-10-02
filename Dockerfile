FROM 279066465364.dkr.ecr.eu-west-1.amazonaws.com/prima-node:26.10.0

USER root

# Node 26 does not bundle corepack, which provides the Yarn version pinned in package.json
RUN npm install -g corepack && \
    corepack enable && \
    mkdir -p /code && \
    chown -R node:node /code

WORKDIR /code

# Serve per avere l'owner dei file scritti dal container uguale all'utente Linux sull'host
USER node

ENTRYPOINT ["/bin/bash"]
