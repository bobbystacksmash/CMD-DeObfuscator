namespace Deobfuscator.Expander

type VarnameContext = {
    Name: string
    Rest: string
    Failed: bool
}

type EnvVars = Map<string,string>

// Put 'type' declarations here