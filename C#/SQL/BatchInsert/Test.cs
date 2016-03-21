using System;
using System.Data;
using System.Data.SqlClient;
using System.Linq;

namespace CSharp2013
{
    internal class Program
    {
        static void InitJobsTable_SqlCommand()
        {
            var connString = new SqlConnectionStringBuilder
            {
                DataSource = @"(localdb)\v11.0",
                InitialCatalog = "TestProgram",
                IntegratedSecurity = true
            }.ToString();

            using (var conn = new SqlConnection(connString))
            {
                conn.Open();

                var cmd = conn.CreateCommand();
                cmd.CommandText = "INSERT INTO Jobs(Status, Payload, Log) VALUES(@Status, @Payload, @Log)";
                var paramStatus = cmd.Parameters.Add("@Status", SqlDbType.NChar, 32);
                var paramPayload = cmd.Parameters.Add("@Payload", SqlDbType.NChar, 128);
                var paramLog = cmd.Parameters.Add("@Log", SqlDbType.NVarChar, int.MaxValue);
                cmd.Prepare();

                var kBatchCount = 12;
                for (var i = 0; i < kBatchCount; ++i)
                {
                    Utility.Timeit("", 1, () =>
                    {
                        using (var trans = conn.BeginTransaction(IsolationLevel.ReadUncommitted))
                        {
                            cmd.Transaction = trans;

                            var count = i < kBatchCount - 2 ? 3000 : 20;
                            paramStatus.Value = i < kBatchCount - 2 ? "Success" : i == kBatchCount - 2 ? "Running" : "Waiting";
                            paramPayload.Value = string.Join(",", Enumerable.Range(0, 10).Select(v => v.ToString()));
                            paramLog.Value = string.Join(",", Enumerable.Range(10000, 1000).Select(v => v.ToString()));

                            for (var j = 0; j < count; ++j)
                            {
                                cmd.ExecuteNonQuery();
                            }

                            trans.Commit();
                        }
                    });
                }
            }
        }

        static void InitJobsTable_SqlDataAdapter()
        {
            var connString = new SqlConnectionStringBuilder
            {
                DataSource = @"(localdb)\v11.0",
                InitialCatalog = "TestProgram",
                IntegratedSecurity = true
            }.ToString();

            using (var conn = new SqlConnection(connString))
            using (var adapter = new SqlDataAdapter())
            using (var table = new DataTable())
            {
                conn.Open();

                var cmd = conn.CreateCommand();
                cmd.CommandText = "INSERT INTO Jobs(Status, Payload, Log) VALUES(@Status, @Payload, @Log)";
                cmd.UpdatedRowSource = UpdateRowSource.None;
                cmd.Parameters.Add("@Status", SqlDbType.NChar, 32, "Status");
                cmd.Parameters.Add("@Payload", SqlDbType.NChar, 128, "Payload");
                cmd.Parameters.Add("@Log", SqlDbType.NVarChar, int.MaxValue, "Log");

                adapter.InsertCommand = cmd;
                adapter.UpdateBatchSize = 4 * 1024;

                table.Columns.Add("Status", typeof(string));
                table.Columns.Add("Payload", typeof(string));
                table.Columns.Add("Log", typeof(string));

                var kBatchCount = 12;
                for (var i = 0; i < kBatchCount; ++i)
                {
                    Utility.Timeit("", 1, () =>
                    {
                        var status = i < kBatchCount - 2 ? "Success" : i == kBatchCount - 2 ? "Running" : "Waiting";
                        var payload = string.Join(",", Enumerable.Range(0, 10).Select(v => v.ToString()));
                        var log = string.Join(",", Enumerable.Range(10000, 1000).Select(v => v.ToString()));
                        var count = i < kBatchCount - 2 ? 3000 : 20;

                        table.Clear();
                        for (var j = 0; j < count; ++j)
                        {
                            table.Rows.Add(status, payload, log);
                        }

                        adapter.Update(table);
                    });
                }
            }
        }

        static void InitJobsTable_SqlBulkCopy()
        {
            var connString = new SqlConnectionStringBuilder
            {
                DataSource = @"(localdb)\v11.0",
                InitialCatalog = "TestProgram",
                IntegratedSecurity = true
            }.ToString();

            using (var bulkCopy = new SqlBulkCopy(connString, SqlBulkCopyOptions.TableLock))
            using (var table = new DataTable())
            {
                bulkCopy.DestinationTableName = "Jobs";
                bulkCopy.ColumnMappings.Add("Status", "Status");
                bulkCopy.ColumnMappings.Add("Payload", "Payload");
                bulkCopy.ColumnMappings.Add("Log", "Log");

                table.Columns.Add("Status", typeof(string));
                table.Columns.Add("Payload", typeof(string));
                table.Columns.Add("Log", typeof(string));

                var kBatchCount = 12;
                for (var i = 0; i < kBatchCount; ++i)
                {
                    Utility.Timeit("", 1, () =>
                    {
                        var status = i < kBatchCount - 2 ? "Success" : i == kBatchCount - 2 ? "Running" : "Waiting";
                        var payload = string.Join(",", Enumerable.Range(0, 10).Select(v => v.ToString()));
                        var log = string.Join(",", Enumerable.Range(10000, 1000).Select(v => v.ToString()));
                        var count = i < kBatchCount - 2 ? 3000 : 20;

                        table.Clear();
                        for (var j = 0; j < count; ++j)
                        {
                            table.Rows.Add(status, payload, log);
                        }

                        bulkCopy.BatchSize = table.Rows.Count;
                        bulkCopy.WriteToServer(table);
                    });
                }
            }
        }

        static void InitItemsTable_SqlBulkCopy()
        {
            var connString = new SqlConnectionStringBuilder
            {
                DataSource = @"(localdb)\v11.0",
                InitialCatalog = "TestProgram",
                IntegratedSecurity = true
            }.ToString();

            using (var bulkCopy = new SqlBulkCopy(connString, SqlBulkCopyOptions.TableLock))
            using (var table = new DataTable())
            {
                bulkCopy.DestinationTableName = "Items";
                bulkCopy.ColumnMappings.Add("Name", "Name");
                bulkCopy.ColumnMappings.Add("Payload", "Payload");
                bulkCopy.ColumnMappings.Add("UserId", "UserId");

                table.Columns.Add("Name", typeof(string));
                table.Columns.Add("Payload", typeof(string));
                table.Columns.Add("UserId", typeof(int));

                var payload = string.Join(",", Enumerable.Range(0, 10).Select(v => v.ToString()));
                var random = new Random();

                var kBatchCount = 3;
                for (var i = 0; i < kBatchCount; ++i)
                {
                    Utility.Timeit("", 1, () =>
                    {
                        table.Clear();
                        for (var j = 0; j < 10000; ++j)
                        {
                            table.Rows.Add("Item_" + (j % 256), payload, random.Next(500));
                        }

                        bulkCopy.BatchSize = table.Rows.Count;
                        bulkCopy.WriteToServer(table);
                    });
                }
            }
        }

        private static void Main(string[] args)
        {
            Utility.Timeit("", 1, () =>
            {
                InitJobsTable_SqlBulkCopy();
            });
            Utility.Timeit("", 1, () =>
            {
                InitItemsTable_SqlBulkCopy(); 
            });
        }
    }
}