schema "main" {
}

table "bangumi" {
  schema = schema.main
  column "id" {
    null           = false
    type           = integer
    auto_increment = true
  }
  column "title_zh" {
    null = true
    type = text
  }
  column "title_jp" {
    null = true
    type = text
  }
  column "season" {
    null = true
    type = integer
  }
  column "cover" {
    null = true
    type = text
  }
  column "group" {
    null = true
    type = text
  }
  column "total_eps" {
    null = true
    type = integer
  }
  column "current_ep" {
    null    = true
    type    = integer
    default = 0
  }
  column "tags" {
    null = true
    type = text
  }
  column "created_at" {
    null    = false
    type    = datetime
    default = sql("CURRENT_TIMESTAMP")
  }
  column "updated_at" {
    null    = false
    type    = datetime
    default = sql("CURRENT_TIMESTAMP")
  }
  primary_key {
    columns = [column.id]
  }
  index "idx_bangumi_title_zh" {
    columns = [column.title_zh]
  }
  index "idx_bangumi_title_jp" {
    columns = [column.title_jp]
  }
  index "idx_bangumi_season" {
    columns = [column.season]
  }
}

table "rss_feed" {
  schema = schema.main
  column "id" {
    null           = false
    type           = integer
    auto_increment = true
  }
  column "bangumi_id" {
    null = false
    type = integer
  }
  column "source" {
    null = false
    type = text
  }
  column "url" {
    null = false
    type = text
  }
  column "keyword" {
    null = true
    type = text
  }
  column "enabled" {
    null    = false
    type    = integer
    default = 1
  }
  column "current_ep" {
    null    = false
    type    = integer
    default = 0
  }
  column "etag" {
    null = true
    type = text
  }
  column "last_modified" {
    null = true
    type = text
  }
  column "last_checked_at" {
    null = true
    type = datetime
  }
  column "created_at" {
    null    = false
    type    = datetime
    default = sql("CURRENT_TIMESTAMP")
  }
  column "updated_at" {
    null    = false
    type    = datetime
    default = sql("CURRENT_TIMESTAMP")
  }
  primary_key {
    columns = [column.id]
  }
  index "idx_rss_feed_bangumi" {
    columns = [column.bangumi_id]
  }
  index "idx_rss_feed_enabled" {
    columns = [column.enabled]
  }
  index "idx_rss_feed_source" {
    columns = [column.source]
  }
  unique "uq_rss_feed_bangumi_url" {
    columns = [column.bangumi_id, column.url]
  }
  foreign_key "fk_rss_bangumi" {
    columns     = [column.bangumi_id]
    ref_columns = [table.bangumi.column.id]
    on_delete   = CASCADE
  }
}
