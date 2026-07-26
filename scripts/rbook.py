#!/usr/bin/env python3
import argparse
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path


DEFAULT_BASE_URL = "https://rbook2.roj.ac.cn"


def load_config_baseurl():
    config_path = Path.home() / ".rbookrc"
    if not config_path.exists():
        return None
    try:
        text = config_path.read_text(encoding="utf-8").strip()
        if not text:
            return None
        if text.startswith("{"):
            data = json.loads(text)
            return data.get("baseurl") or data.get("base_url") or data.get("RBOOK_BASE_URL")
        for line in text.splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if "=" in line:
                key, value = line.split("=", 1)
                if key.strip().lower() in {"baseurl", "base_url", "rbook_base_url"}:
                    return value.strip().strip('"').strip("'")
            else:
                return line
    except Exception:
        return None
    return None


def resolve_baseurl(cli_value):
    if cli_value:
        return cli_value.rstrip("/")
    env_value = os.environ.get("RBOOK_BASE_URL")
    if env_value:
        return env_value.rstrip("/")
    config_value = load_config_baseurl()
    if config_value:
        return config_value.rstrip("/")
    return DEFAULT_BASE_URL


def request_json(baseurl, path, params=None):
    query = urllib.parse.urlencode(params or {}, doseq=True)
    url = f"{baseurl}{path}"
    if query:
        url = f"{url}?{query}"
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            body = resp.read().decode("utf-8")
            return json.loads(body)
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        try:
            data = json.loads(body)
        except Exception:
            data = {"error": "HTTP_ERROR", "message": body}
        print(json.dumps(data, ensure_ascii=False, indent=2), file=sys.stderr)
        sys.exit(1)
    except Exception as exc:
        print(json.dumps({"error": "REQUEST_FAILED", "message": str(exc)}, ensure_ascii=False, indent=2), file=sys.stderr)
        sys.exit(1)


def print_json(data, pretty=True):
    if pretty:
        print(json.dumps(data, ensure_ascii=False, indent=2))
    else:
        print(json.dumps(data, ensure_ascii=False, separators=(",", ":")))


def positive_int(value):
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("must be a positive integer")
    return number


def find_pages(items, query, limit=20):
    words = [word.casefold() for word in query.split() if word]
    matches = []

    for item in items:
        page_id = str(item.get("id") or "").casefold()
        title = str(item.get("title") or "").casefold()
        description = str(item.get("description") or "").casefold()
        tags = [str(tag).casefold() for tag in item.get("tags") or []]
        score = 0

        for word in words:
            word_scores = []
            if word in page_id:
                word_scores.append(400 + (100 if word == page_id else 0))
            if word in title:
                word_scores.append(300 + (100 if word == title else 0))
            if any(word in tag for tag in tags):
                word_scores.append(200)
            if word in description:
                word_scores.append(100)
            if not word_scores:
                break
            score += max(word_scores)
        else:
            matches.append((score, title, page_id, item))

    matches.sort(key=lambda match: (-match[0], match[1], match[2]))
    return {
        "query": query,
        "total": len(matches),
        "items": [match[3] for match in matches[:limit]],
    }


def main():
    parser = argparse.ArgumentParser(description="rbook HTTP API client")
    parser.add_argument("--baseurl", help="API base url")
    parser.add_argument("--pretty", action="store_true", default=True, help="pretty print JSON")
    parser.add_argument("--compact-json", action="store_true", help="compact JSON output")

    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("health")
    sub.add_parser("site")

    p_catalog = sub.add_parser("catalog")
    p_catalog.add_argument("--compact", action="store_true")

    p_find = sub.add_parser("find")
    p_find.add_argument("query")
    p_find.add_argument("--limit", type=positive_int, default=20)

    p_pages = sub.add_parser("pages")
    p_pages.add_argument("--id")
    p_pages.add_argument("--tag")
    p_pages.add_argument("--limit", type=int)
    p_pages.add_argument("--offset", type=int)

    p_codes = sub.add_parser("codes")
    p_codes.add_argument("--id")
    p_codes.add_argument("--tag")
    p_codes.add_argument("--content", action="store_true")
    p_codes.add_argument("--limit", type=int)
    p_codes.add_argument("--offset", type=int)

    p_code = sub.add_parser("code")
    p_code.add_argument("id")
    p_code.add_argument("--content", action="store_true")

    sub.add_parser("tags")

    args = parser.parse_args()
    baseurl = resolve_baseurl(args.baseurl)
    pretty = not args.compact_json

    if args.command == "health":
        print_json(request_json(baseurl, "/api/health"), pretty)
    elif args.command == "site":
        print_json(request_json(baseurl, "/api/site"), pretty)
    elif args.command == "catalog":
        params = {}
        if args.compact:
            params["compact"] = "true"
        print_json(request_json(baseurl, "/api/catalog", params), pretty)
    elif args.command == "find":
        catalog = request_json(baseurl, "/api/catalog", {"compact": "true"})
        print_json(find_pages(catalog.get("items") or [], args.query, args.limit), pretty)
    elif args.command == "pages":
        params = {}
        if args.id:
            params["id"] = args.id
        if args.tag:
            params["tag"] = args.tag
        if args.limit is not None:
            params["limit"] = str(args.limit)
        if args.offset is not None:
            params["offset"] = str(args.offset)
        print_json(request_json(baseurl, "/api/pages", params), pretty)
    elif args.command == "codes":
        params = {}
        if args.id:
            params["id"] = args.id
        if args.tag:
            params["tag"] = args.tag
        if args.content:
            params["includeContent"] = "true"
        if args.limit is not None:
            params["limit"] = str(args.limit)
        if args.offset is not None:
            params["offset"] = str(args.offset)
        print_json(request_json(baseurl, "/api/codes", params), pretty)
    elif args.command == "code":
        params = {"id": args.id}
        if args.content:
            params["includeContent"] = "true"
        print_json(request_json(baseurl, "/api/codes", params), pretty)
    elif args.command == "tags":
        print_json(request_json(baseurl, "/api/tags"), pretty)
    else:
        parser.error(f"unknown command: {args.command}")


if __name__ == "__main__":
    main()
