using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

public static class JsonOperation
{
    private static JsonSerializerSettings sJsonSerializeSetting = new JsonSerializerSettings()
    {
        TypeNameHandling = TypeNameHandling.Auto,
        Formatting = Formatting.Indented,
    };

    public static string Serialize<T>(T obj)
    {
        return JsonConvert.SerializeObject(obj, typeof(T), sJsonSerializeSetting);
    }
    public static T Deserialize<T>(string json)
    {
        return JsonConvert.DeserializeObject<T>(json, sJsonSerializeSetting);
    }
    public static string SerializeSheets(IEnumerable<DataSheet> sheets)
    {
        return Serialize(sheets.ToList());
    }
    public static List<DataSheet> DeserializeSheets(string s)
    {
        return Deserialize<List<DataSheet>>(s);
    }
}
