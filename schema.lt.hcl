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
  column "rss" {
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
