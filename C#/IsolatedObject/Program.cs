using System;
using System.Collections.Generic;
using System.Runtime.Remoting.Lifetime;
using System.Security.Permissions;
using System.Threading;
using System.Threading.Tasks;

namespace CSharp2013
{
    public sealed class IsolatedObject<T> : IDisposable where T : MarshalByRefObject
    {
        [Serializable]
        public sealed class InfinityLifetimeSponsor : ISponsor
        {
            [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.Infrastructure)]
            public TimeSpan Renewal(ILease lease)
            {
                return TimeSpan.FromMinutes(10);
            }
        }

        private AppDomain mAppDomain;
        private InfinityLifetimeSponsor mSponsor = new InfinityLifetimeSponsor();

        public IsolatedObject()
        {
            mAppDomain = AppDomain.CreateDomain(
                "IsolatedObject_" + typeof(T).Name + "_" + Guid.NewGuid(),
                AppDomain.CurrentDomain.Evidence,
                AppDomain.CurrentDomain.SetupInformation);

            Proxy = (T) mAppDomain.CreateInstanceAndUnwrap(typeof (T).Assembly.FullName, typeof (T).FullName);
            var lease = Proxy.GetLifetimeService() as ILease;
            if (lease != null) lease.Register(mSponsor);
        }

        public T Proxy { get; private set; }

        public void Dispose()
        {
            if (mAppDomain != null)
            {
                var lease = Proxy.GetLifetimeService() as ILease;
                if (lease != null) lease.Unregister(mSponsor);

                AppDomain.Unload(mAppDomain);
                mAppDomain = null;
            }
        }
    }

    public class Library
    {
        public static int Value;

        public static void Inc()
        {
            ++Value;
        }

        public static void Print()
        {
            Console.WriteLine(Value);
        }
    }

    public class LibraryWrapper : MarshalByRefObject
    {
        public void Inc()
        {
            Library.Inc();
        }

        public void Print()
        {
            Library.Print();
        }
    }

    internal class Program
    {
        private static void Main(string[] args)
        {
            using (var wrapper1 = new IsolatedObject<LibraryWrapper>())
            using (var wrapper2 = new IsolatedObject<LibraryWrapper>())
            {
                wrapper1.Proxy.Inc();
                wrapper1.Proxy.Inc();
                wrapper1.Proxy.Inc();
                wrapper2.Proxy.Inc();
                wrapper2.Proxy.Inc();

                wrapper1.Proxy.Print();
                wrapper2.Proxy.Print();
            }
        }
    }
}