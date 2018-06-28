namespace MicroCosmo

type CompilerException(message : string) as this =
    inherit System.Exception(message)
    override x.ToString() = this.GetType().Name + ": " + this.Message