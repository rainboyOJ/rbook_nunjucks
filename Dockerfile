FROM node:22-bookworm-slim AS deps
WORKDIR /app
COPY package.json package-lock.json ./
COPY apps/algorithm-book/package.json ./apps/algorithm-book/package.json
COPY packages/rbook-cli/package.json ./packages/rbook-cli/package.json
COPY packages/rbook-core/package.json ./packages/rbook-core/package.json
COPY packages/rbook-markdown/package.json ./packages/rbook-markdown/package.json
COPY packages/rbook-search/package.json ./packages/rbook-search/package.json
COPY packages/rbook-server/package.json ./packages/rbook-server/package.json
RUN npm ci --legacy-peer-deps

FROM node:22-bookworm-slim AS runtime
WORKDIR /app
ENV NODE_ENV=production
ENV HOST=0.0.0.0
ENV PORT=3000

RUN apt-get update \
  && apt-get install -y --no-install-recommends graphviz python3 \
  && rm -rf /var/lib/apt/lists/*

COPY --from=deps /app/node_modules ./node_modules
COPY . .

RUN npm run build:all

EXPOSE 3000
CMD ["npm", "run", "serve"]
