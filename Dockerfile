FROM haskell:9.6.7 AS build

WORKDIR /app

RUN apt-get update && apt-get install -y postgresql-server-dev-14 && rm -rf /var/lib/apt/lists/*

COPY projetoHello.cabal cabal.project ./  

RUN cabal update

COPY . .

RUN cabal build --enable-relocatable

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y libgmp10 libpq5 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/dist-newstyle/build/*/*/projetoHello-*/x/projetoHello/build/projetoHello .

EXPOSE 8080

CMD ["/app/projetoHello"]
