import ky from "ky";
import type { BgmSubject } from "./types";

// Base API configuration
const httpClient = ky.create({
  prefixUrl: "/api",
  timeout: 30000,
  headers: {
    "Content-Type": "application/json",
  },
  hooks: {
    beforeError: [
      (error) => {
        const { response } = error;
        if (response?.body) {
          error.name = "HTTPError";
          error.message = `${response.status} ${response.statusText}`;
        }
        return error;
      },
    ],
  },
});

async function searchBangumi(keyword: string) {
  return httpClient
    .get("search/bangumi", {
      searchParams: {
        keyword,
      },
    })
    .json<BgmSubject[]>();
}

export { searchBangumi };
