// Example usage of the generated API client
import { ApiClient } from './generated';
import type { CreateBangumiRequest, BgmBangumi } from './generated';

// Example: Search for bangumi from external sources
export async function searchBangumi(keyword: string) {
  try {
    const results = await ApiClient.search.searchBangumi(keyword);
    console.log('Search results:', results);
    return results;
  } catch (error) {
    console.error('Error searching bangumi:', error);
    throw error;
  }
}

// Example: Parse bangumi title
export async function parseBangumiTitle(titleText: string): Promise<BgmBangumi> {
  try {
    const result = await ApiClient.bangumi.parse({ titleText });
    console.log('Parsed bangumi:', result);
    return result;
  } catch (error) {
    console.error('Error parsing bangumi title:', error);
    throw error;
  }
}

// Example: Create new bangumi in local database
export async function createBangumi(bangumiData: CreateBangumiRequest) {
  try {
    const result = await ApiClient.bangumi.create(bangumiData);
    console.log('Created bangumi:', result);
    return result;
  } catch (error) {
    console.error('Error creating bangumi:', error);
    throw error;
  }
}

// Example: Get all bangumi from local database
export async function getAllBangumi() {
  try {
    const bangumi = await ApiClient.bangumi.getAll();
    console.log('All bangumi:', bangumi);
    return bangumi;
  } catch (error) {
    console.error('Error fetching bangumi:', error);
    throw error;
  }
}

// Example: Workflow - Search, parse, and create
export async function searchParseAndCreate(searchKeyword: string) {
  try {
    // 1. Search for bangumi
    const searchResults = await searchBangumi(searchKeyword);
    if (searchResults.length === 0) {
      throw new Error('No bangumi found');
    }

    const firstResult = searchResults[0];
    
    // 2. Parse the title to extract season info
    const parsedTitle = await parseBangumiTitle(firstResult.name);
    
    // 3. Create bangumi in local database
    const newBangumiData: CreateBangumiRequest = {
      titleZh: firstResult.name_cn || firstResult.name,
      titleJp: firstResult.name,
      season: parsedTitle.season,
      cover: firstResult.images.large,
      group: '', // Would need to be filled from other sources
      totalEps: firstResult.eps_count || 0,
      currentEp: 0,
      tags: firstResult.tags.map(tag => tag.name),
      rss: '', // Would need to be filled from RSS search
    };
    
    const createdBangumi = await createBangumi(newBangumiData);
    return createdBangumi;
    
  } catch (error) {
    console.error('Error in workflow:', error);
    throw error;
  }
}