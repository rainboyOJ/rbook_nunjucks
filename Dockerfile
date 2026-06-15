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
ENV RBOOK_CONTENT_DIR=/content
ENV RBOOK_RUNTIME_DIR=/tmp/rbook-runtime

RUN apt-get update \
  && apt-get install -y --no-install-recommends graphviz python3 \
  && rm -rf /var/lib/apt/lists/*

COPY --from=deps /app/node_modules ./node_modules
COPY package.json package-lock.json tsconfig.base.json ./
COPY bin ./bin
COPY build_all_dot_file.py ./build_all_dot_file.py
COPY src/online_judge ./src/online_judge
COPY packages ./packages
COPY apps/algorithm-book/package.json ./apps/algorithm-book/package.json
COPY apps/algorithm-book/theme ./apps/algorithm-book/theme
COPY apps/algorithm-book/public ./apps/algorithm-book/public
COPY apps/algorithm-book/markdown-style ./apps/algorithm-book/markdown-style
COPY apps/algorithm-book/third_part ./apps/algorithm-book/third_part

RUN npm run build:packages

EXPOSE 3000
CMD ["npm", "run", "start:runtime"]
