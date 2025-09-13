import {
  CommandDialog,
  CommandInput,
  CommandList,
  CommandEmpty,
  CommandGroup,
  CommandItem,
  CommandLoading,
} from "@/components/ui/command";
import { useState } from "react";
import { useDebounceValue } from "usehooks-ts";
import { useSearchBangumi } from "../query";

interface Props {
  open: boolean;
  setOpen: (open: boolean) => void;
}

export function SearchDialog({ open, setOpen }: Props) {
  const [searchValue, setSearchValue] = useDebounceValue("", 800);
  const [selectedResult, setSelectedResult] = useState<any>(null);

  const { data, isLoading, error } = useSearchBangumi(searchValue);

  console.log(data);

  return (
    <CommandDialog open={open} onOpenChange={setOpen} className="h-[600px]">
      <CommandInput
        placeholder="Search for bangumi..."
        onValueChange={setSearchValue}
      />
      <div className="flex flex-1 min-h-0">
        <div className="flex-1 border-r min-h-0">
          <CommandList className="h-full max-h-none overflow-auto">
            {isLoading && <CommandLoading>Fetching</CommandLoading>}
            {data.map((item: any) => (
              <CommandItem
                key={item.id}
                value={item}
                onSelect={() => setSelectedResult(item)}
                className={selectedResult?.id === item.id ? "bg-accent" : ""}
              >
                <div className="flex flex-col gap-1">
                  <span className="font-medium">
                    {item.name || item.name_cn}
                  </span>
                </div>
              </CommandItem>
            ))}
          </CommandList>
        </div>
        <div className="flex-1 p-4 overflow-auto min-h-0">
          {selectedResult ? (
            <div className="space-y-4">
              <div>
                <h3 className="text-lg font-semibold">
                  {selectedResult.name || selectedResult.name_cn}
                </h3>
                {selectedResult.name_cn && selectedResult.name && (
                  <p className="text-sm text-muted-foreground">
                    {selectedResult.name}
                  </p>
                )}
              </div>
              <div className="space-y-2 text-sm">
                <div className="flex gap-2">
                  <span className="font-medium">Type:</span>
                  <span>{selectedResult.type_name}</span>
                </div>
                <div className="flex gap-2">
                  <span className="font-medium">Air Date:</span>
                  <span>{selectedResult.air_date}</span>
                </div>
                {selectedResult.rating && (
                  <div className="flex gap-2">
                    <span className="font-medium">Rating:</span>
                    <span>{selectedResult.rating.score}</span>
                  </div>
                )}
              </div>
              {selectedResult.summary && (
                <div>
                  <h4 className="font-medium mb-2">Summary</h4>
                  <p className="text-sm text-muted-foreground leading-relaxed">
                    {selectedResult.summary}
                  </p>
                </div>
              )}
            </div>
          ) : (
            <div className="flex items-center justify-center h-full text-muted-foreground">
              Select an item to view details
            </div>
          )}
        </div>
      </div>
    </CommandDialog>
  );
}
