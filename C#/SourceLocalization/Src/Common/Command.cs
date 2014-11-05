using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

public abstract class Command
{
    public bool Active { get; set; }
    public abstract void Execute();

    public static List<Command> CreateTemplates()
    {
        var commands = from type in Assembly.GetCallingAssembly().GetTypes()
                       where type.IsSubclassOf(typeof(Command))
                       select type.GetMethod("CreateTemplate").Invoke(null, null) as Command;
        return commands.ToList();
    }
    public static void RunMain(string[] args)
    {
        if (args.Length == 0)
        {
            string commandsFile = "Commands.json";
            Console.Error.WriteLine("Usage: {0} {1}\n", Path.GetFileName(Assembly.GetExecutingAssembly().Location), commandsFile);

            File.WriteAllText(commandsFile, JsonOperation.Serialize(CreateTemplates()), Encoding.UTF8);
            Console.Error.WriteLine("(Command templates have wrote to {0}...)", commandsFile);

            Console.ReadKey();
            return;
        }

        var cmds = JsonOperation.Deserialize<List<Command>>(File.ReadAllText(args[0], Encoding.UTF8)).Where(cmd => cmd.Active);
        Utils.Timeit(1, () =>
        {
            foreach (var cmd in cmds) cmd.Execute();
        });
    }
}