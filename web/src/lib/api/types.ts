// API Response Types
export interface BgmSubject {
  id: number;
  name: string;
  name_cn: string;
  summary: string;
  images: {
    large: string;
    common: string;
    medium: string;
    small: string;
    grid: string;
  };
  eps: number;
}

export interface ApiError {
  error: string;
}
