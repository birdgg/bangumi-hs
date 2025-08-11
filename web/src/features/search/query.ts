import { searchBangumi } from "@/lib/api";
import { useQuery } from "@tanstack/react-query";

export const useSearchBangumi = (q: string) => {
  return useQuery({
    queryKey: ["search-bangumi", q],
    queryFn: () => searchBangumi(q),
    enabled: !!q,
    initialData: [],
  });
};
