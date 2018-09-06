namespace MicroCosmo

type CompilerException(message : string) as this =
    inherit System.Exception(message)
    override x.ToString() = 
        sprintf "%s: %s" (this.GetType().Name) this.Message