# PROPOSAL

```json
{
  "metrics": [{
    "id": "M003",
    "name": "平均电价",
    "components": [
      {"id": "M001", "conditions": ["南京市", "2024", "大工业用户"]},
      {"id": "M002", "conditions": ["南京市", "2024", "大工业用户"]}
    ]
  }],
  "conditions": [
    {"type": "地域", "value": "南京市"},
    {"type": "时间", "value": "2024"},
    {"type": "用户类型", "value": "大工业用户"}
  ]
}

// SELECT
//   SUM(电费) / SUM(电量) AS 平均电价
// FROM 电费表
// WHERE
//   地区 = '南京市'
//   AND 年份 = 2024
//   AND 用户类型 = '大工业用户'
```

