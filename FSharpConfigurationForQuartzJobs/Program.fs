open System
open Microsoft.FSharp.Reflection
open Autofac
open System.Threading
open System.Threading.Tasks

type IWorkflow = 
     abstract member RunAsync: unit -> Task

type OrderCreationWorkflow() = interface IWorkflow 
                                with member this.RunAsync(): Task = Task.FromResult<Object>(null) :> Task

type JetSpecificOrderCreationWorkflow() = interface IWorkflow
                                            with member this.RunAsync(): Task = 
                                                    printfn "workflow.Run()"
                                                    Task.FromResult<Object>(null) :> Task

type JetUniqueWorkflow() = interface IWorkflow 
                            with member this.RunAsync(): Task = Task.FromResult<Object>(null) :> Task

type OrderR1CreationWorkflow() = interface IWorkflow 
                                    with member this.RunAsync(): Task = Task.FromResult<Object>(null) :> Task

type ReturnCreationWorkflow() = interface IWorkflow 
                                    with member this.RunAsync(): Task = Task.FromResult<Object>(null) :> Task

type WorkflowJob<'T when 'T :> IWorkflow>(t: 'T) = 
        member this.Run() = printfn "job.Run()"
                            async { do! t.RunAsync() |> Async.AwaitTask }

type OrderCreationJob<'T when 'T :> IWorkflow>(t: 'T) = class inherit WorkflowJob<'T>(t) end
type OrderR1CreationJob<'T when 'T :> IWorkflow>(t: 'T) = class inherit WorkflowJob<'T>(t) end
type ReturnCreationJob<'T when 'T :> IWorkflow>(t: 'T) = class inherit WorkflowJob<'T>(t) end
type JetUniqueJob<'T when 'T :> IWorkflow>(t: 'T) = class inherit WorkflowJob<'T>(t) end

type Config = 
      | JetJobs  of OrderCreationJob<JetSpecificOrderCreationWorkflow> *
                    JetUniqueJob<JetUniqueWorkflow>
      | EBayJobs
      | CommonJobs of OrderCreationJob<OrderCreationWorkflow> *
                      OrderR1CreationJob<OrderR1CreationWorkflow> *
                      ReturnCreationJob<ReturnCreationWorkflow>

[<EntryPoint>]
let main argv = 

    let builder = new ContainerBuilder()

    let rec register (level: int)(t: Type) =
        printfn "%s %A" (String.replicate(level)("----") + ">")(t.Name)
        builder.RegisterType(t).PreserveExistingDefaults() |> ignore
        if t.IsGenericType
        then let nextLevelReg = register (level + 1)
             t.GetGenericArguments() |> Seq.iter nextLevelReg

    let z = FSharpType.GetUnionCases(typedefof<Config>) 
            |> Seq.iter (fun e -> e.GetFields() 
                                  |> Seq.iter (fun q -> register 0 q.PropertyType))

    let container = builder.Build()
    
    printfn "\nrunning..."

    let job = container.Resolve<OrderCreationJob<JetSpecificOrderCreationWorkflow>>()

    Async.RunSynchronously(job.Run())

    0 // return an integer exit code
