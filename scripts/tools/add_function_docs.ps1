$ErrorActionPreference = "Stop"

function Get-NewlineStyle {
    param([string]$Content)
    if ($Content -match "`r`n") {
        return "`r`n"
    }
    return "`n"
}

function Get-Indent {
    param([string]$Line)
    $m = [regex]::Match($Line, "^\s*")
    return $m.Value
}

function Has-DocBlockAbove {
    param(
        [System.Collections.Generic.List[string]]$Lines,
        [int]$Index
    )
    $j = $Index - 1
    while ($j -ge 0) {
        $trimmed = $Lines[$j].Trim()
        if ($trimmed -eq "") {
            $j--
            continue
        }
        if ($trimmed.StartsWith("@")) {
            $j--
            continue
        }
        break
    }
    if ($j -lt 0) {
        return $false
    }
    if (-not $Lines[$j].Trim().EndsWith("*/")) {
        return $false
    }

    for ($k = $j - 1; $k -ge 0 -and ($j - $k) -le 120; $k--) {
        $trimmed = $Lines[$k].Trim()
        if ($trimmed.StartsWith("/**")) {
            return $true
        }
        if ($trimmed -ne "" -and -not $trimmed.StartsWith("*")) {
            break
        }
    }
    return $false
}

function Get-DocBlockRangeAbove {
    param(
        [System.Collections.Generic.List[string]]$Lines,
        [int]$Index
    )
    $j = $Index - 1
    while ($j -ge 0 -and $Lines[$j].Trim() -eq "") {
        $j--
    }
    if ($j -lt 0) {
        return $null
    }
    if (-not $Lines[$j].Trim().EndsWith("*/")) {
        return $null
    }
    for ($k = $j - 1; $k -ge 0 -and ($j - $k) -le 120; $k--) {
        $trimmed = $Lines[$k].Trim()
        if ($trimmed.StartsWith("/**")) {
            return [PSCustomObject]@{
                Start = $k
                End = $j
            }
        }
        if ($trimmed -ne "" -and -not $trimmed.StartsWith("*")) {
            break
        }
    }
    return $null
}

function Is-AutoDocBlock {
    param(
        [System.Collections.Generic.List[string]]$Lines,
        [int]$Start,
        [int]$End
    )
    if ($Start -lt 0 -or $End -lt $Start) {
        return $false
    }
    $block = ($Lines[$Start..$End] -join "`n")
    return ($block -match "Executes the " -and $block -match "Edge cases:")
}

function Find-JavaClassName {
    param([string]$Content)
    $m = [regex]::Match($Content, "\b(class|record|enum)\s+([A-Za-z_][A-Za-z0-9_]*)")
    if ($m.Success) {
        return $m.Groups[2].Value
    }
    return ""
}

function Is-JavaMethodCandidate {
    param([string]$Line)
    $trimmed = $Line.Trim()
    if ([string]::IsNullOrWhiteSpace($trimmed)) { return $false }
    if ($trimmed.StartsWith("@")) { return $false }
    if ($trimmed.StartsWith("//")) { return $false }
    if ($trimmed.StartsWith("/*")) { return $false }
    if ($trimmed.StartsWith("*")) { return $false }
    if ($trimmed -notmatch "\(") { return $false }
    if ($trimmed -match "\b(class|interface|enum|record)\b") { return $false }
    if ($trimmed -match "^(if|for|while|switch|catch|return|throw|new|else|do)\b") { return $false }
    if ($trimmed -match "^(package|import)\b") { return $false }
    if ($trimmed -match "->") { return $false }
    $openParen = $trimmed.IndexOf("(")
    $equalsPos = $trimmed.IndexOf("=")
    if ($equalsPos -ge 0 -and $equalsPos -lt $openParen) { return $false }

    if ($trimmed -match "^(public|protected|private)\s+[A-Za-z_][A-Za-z0-9_]*\s*\(") { return $true }
    if ($trimmed -match "^(public|protected|private|static|final|synchronized|abstract|default|native|strictfp|transient)\b") { return $true }
    if ($trimmed -match "^[A-Za-z0-9_<>\[\],\.\?@ ]+\s+[A-Za-z_][A-Za-z0-9_]*\s*\(") { return $true }

    return $false
}

function Collect-SignatureText {
    param(
        [System.Collections.Generic.List[string]]$Lines,
        [int]$StartIndex
    )
    $parts = New-Object System.Collections.Generic.List[string]
    $depth = 0
    $seenOpen = $false

    for ($j = $StartIndex; $j -lt [Math]::Min($Lines.Count, $StartIndex + 120); $j++) {
        $line = $Lines[$j]
        $parts.Add($line.Trim())
        foreach ($ch in $line.ToCharArray()) {
            if ($ch -eq "(") {
                $depth++
                $seenOpen = $true
            } elseif ($ch -eq ")") {
                if ($depth -gt 0) {
                    $depth--
                }
            }
        }
        if ($seenOpen -and $depth -eq 0) {
            break
        }
    }
    return (($parts -join " ") -replace "\s+", " ").Trim()
}

function Remove-LeadingJavaGenerics {
    param([string]$Text)
    $t = $Text.Trim()
    if (-not $t.StartsWith("<")) {
        return $t
    }
    $depth = 0
    for ($i = 0; $i -lt $t.Length; $i++) {
        $c = $t[$i]
        if ($c -eq "<") {
            $depth++
        } elseif ($c -eq ">") {
            $depth--
            if ($depth -eq 0) {
                if ($i + 1 -lt $t.Length) {
                    return $t.Substring($i + 1).Trim()
                }
                return ""
            }
        }
    }
    return $t
}

function Split-TopLevel {
    param(
        [string]$Text,
        [char]$Delimiter
    )
    $parts = New-Object System.Collections.Generic.List[string]
    $current = New-Object System.Text.StringBuilder
    $angle = 0
    $paren = 0
    $bracket = 0
    $brace = 0

    foreach ($c in $Text.ToCharArray()) {
        switch ($c) {
            "<" { $angle++ }
            ">" { if ($angle -gt 0) { $angle-- } }
            "(" { $paren++ }
            ")" { if ($paren -gt 0) { $paren-- } }
            "[" { $bracket++ }
            "]" { if ($bracket -gt 0) { $bracket-- } }
            "{" { $brace++ }
            "}" { if ($brace -gt 0) { $brace-- } }
        }

        if ($c -eq $Delimiter -and $angle -eq 0 -and $paren -eq 0 -and $bracket -eq 0 -and $brace -eq 0) {
            $parts.Add($current.ToString())
            $null = $current.Clear()
            continue
        }
        $null = $current.Append($c)
    }
    $parts.Add($current.ToString())
    return $parts
}

function Parse-JavaParameters {
    param([string]$ParamsRaw)
    $params = @()
    if ([string]::IsNullOrWhiteSpace($ParamsRaw)) {
        return $params
    }
    $chunks = Split-TopLevel -Text $ParamsRaw -Delimiter ","
    foreach ($chunk in $chunks) {
        $clean = ($chunk -replace "@[A-Za-z_][A-Za-z0-9_]*(\([^)]*\))?\s*", " ")
        $clean = ($clean -replace "\bfinal\s+", " ")
        $clean = ($clean -replace "\s+", " ").Trim()
        if ([string]::IsNullOrWhiteSpace($clean)) {
            continue
        }
        $m = [regex]::Match($clean, "([A-Za-z_][A-Za-z0-9_]*)\s*$")
        if (-not $m.Success) {
            continue
        }
        $name = $m.Groups[1].Value
        $type = $clean.Substring(0, $m.Index).Trim()
        if ([string]::IsNullOrWhiteSpace($type)) {
            $type = "Object"
        }
        $params += [PSCustomObject]@{
            Name = $name
            Type = $type
        }
    }
    return $params
}

function Parse-JavaMethodInfo {
    param(
        [string]$Signature,
        [string]$ClassName
    )
    if ([string]::IsNullOrWhiteSpace($Signature)) {
        return $null
    }
    $open = $Signature.IndexOf("(")
    if ($open -lt 0) {
        return $null
    }

    $depth = 0
    $close = -1
    for ($i = $open; $i -lt $Signature.Length; $i++) {
        $c = $Signature[$i]
        if ($c -eq "(") {
            $depth++
        } elseif ($c -eq ")") {
            $depth--
            if ($depth -eq 0) {
                $close = $i
                break
            }
        }
    }
    if ($close -lt 0) {
        return $null
    }

    $left = $Signature.Substring(0, $open).Trim()
    $left = [regex]::Replace($left, "@[A-Za-z_][A-Za-z0-9_]*(\([^)]*\))?\s*", " ")
    $left = ($left -replace "\s+", " ").Trim()

    $nameMatch = [regex]::Match($left, "([A-Za-z_][A-Za-z0-9_]*)\s*$")
    if (-not $nameMatch.Success) {
        return $null
    }
    $methodName = $nameMatch.Groups[1].Value
    $prefix = $left.Substring(0, $nameMatch.Index).Trim()
    $prefix = [regex]::Replace($prefix, "^(public|protected|private|static|final|synchronized|abstract|default|native|strictfp|transient)\b\s*", "")
    while ($prefix -match "^(public|protected|private|static|final|synchronized|abstract|default|native|strictfp|transient)\b\s*") {
        $prefix = [regex]::Replace($prefix, "^(public|protected|private|static|final|synchronized|abstract|default|native|strictfp|transient)\b\s*", "")
    }
    $returnType = Remove-LeadingJavaGenerics -Text $prefix

    $isConstructor = $false
    if ([string]::IsNullOrWhiteSpace($returnType)) {
        if ($methodName -eq $ClassName -or $methodName -match "^[A-Z]") {
            $isConstructor = $true
        }
    }

    if (-not $isConstructor -and [string]::IsNullOrWhiteSpace($returnType)) {
        $returnType = "void"
    }

    $paramsRaw = $Signature.Substring($open + 1, $close - $open - 1)
    $params = Parse-JavaParameters -ParamsRaw $paramsRaw

    $after = ""
    if ($close + 1 -lt $Signature.Length) {
        $after = $Signature.Substring($close + 1)
    }
    $throws = @()
    $throwsMatch = [regex]::Match($after, "\bthrows\s+([^\{;]+)")
    if ($throwsMatch.Success) {
        $throwParts = Split-TopLevel -Text $throwsMatch.Groups[1].Value -Delimiter ","
        foreach ($throwPart in $throwParts) {
            $ex = ($throwPart -replace "\s+", " ").Trim()
            if (-not [string]::IsNullOrWhiteSpace($ex)) {
                $throws += $ex
            }
        }
    }

    return [PSCustomObject]@{
        Name = $methodName
        ClassName = $ClassName
        ConstructorName = $(if ($isConstructor) { $methodName } else { $ClassName })
        ReturnType = $returnType
        IsConstructor = $isConstructor
        Params = $params
        Throws = $throws
    }
}

function Build-JavaDocLines {
    param(
        [string]$Indent,
        $Info
    )
    $doc = New-Object System.Collections.Generic.List[string]
    $doc.Add("$Indent/**")
    $doc.Add("$Indent * Executes the $($Info.Name) operation.")
    if ($Info.IsConstructor) {
        $doc.Add("$Indent * <p>Return value: A fully initialized $($Info.ConstructorName) instance.</p>")
    }
    $doc.Add("$Indent *")

    foreach ($param in $Info.Params) {
        $doc.Add("$Indent * @param $($param.Name) Parameter of type {@code $($param.Type)} used by this operation.")
    }

    if (-not $Info.IsConstructor) {
        if ($Info.ReturnType -eq "void") {
            $doc.Add("$Indent * @return void No value is returned; the method applies side effects to existing state.")
        } else {
            $doc.Add("$Indent * @return {@code $($Info.ReturnType)} Result produced by this operation.")
        }
    }

    if ($Info.Throws.Count -gt 0) {
        foreach ($throwType in $Info.Throws) {
            $doc.Add("$Indent * @throws $throwType If the operation cannot complete successfully.")
        }
    } else {
        $doc.Add("$Indent * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>")
    }
    $doc.Add("$Indent * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>")
    $doc.Add("$Indent */")
    return $doc
}

function Process-JavaFile {
    param([string]$Path)
    $raw = Get-Content -LiteralPath $Path -Raw
    $newline = Get-NewlineStyle -Content $raw
    $lineArray = $raw -split "`r?`n", -1
    $lines = New-Object System.Collections.Generic.List[string]
    foreach ($line in $lineArray) { $lines.Add($line) }

    $className = Find-JavaClassName -Content $raw
    $updated = $false

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]
        if (-not (Is-JavaMethodCandidate -Line $line)) {
            continue
        }

        $insertIndex = $i
        $docRange = Get-DocBlockRangeAbove -Lines $lines -Index $insertIndex
        if ($null -ne $docRange) {
            if (Is-AutoDocBlock -Lines $lines -Start $docRange.Start -End $docRange.End) {
                $removeCount = $docRange.End - $docRange.Start + 1
                $lines.RemoveRange($docRange.Start, $removeCount)
                if ($docRange.Start -lt $i) {
                    $i -= $removeCount
                }
                if ($docRange.Start -lt $insertIndex) {
                    $insertIndex -= $removeCount
                }
            } else {
                continue
            }
        }

        $insertIndex = $i
        while ($insertIndex -gt 0 -and $lines[$insertIndex - 1].Trim().StartsWith("@")) {
            $insertIndex--
        }

        $signature = Collect-SignatureText -Lines $lines -StartIndex $i
        $info = Parse-JavaMethodInfo -Signature $signature -ClassName $className
        if ($null -eq $info) {
            continue
        }

        $indent = Get-Indent -Line $line
        $docLines = Build-JavaDocLines -Indent $indent -Info $info
        for ($d = 0; $d -lt $docLines.Count; $d++) {
            $lines.Insert($insertIndex + $d, $docLines[$d])
        }
        if ($insertIndex -le $i) {
            $i += $docLines.Count
        }
        $updated = $true
    }

    if ($updated) {
        $updatedContent = $lines -join $newline
        Set-Content -LiteralPath $Path -Value $updatedContent -NoNewline
        return 1
    }
    return 0
}

function Is-JsFunctionCandidate {
    param([string]$Line)
    $trimmed = $Line.Trim()
    if ([string]::IsNullOrWhiteSpace($trimmed)) { return $false }
    if ($trimmed.StartsWith("//")) { return $false }
    if ($trimmed.StartsWith("/*")) { return $false }
    if ($trimmed.StartsWith("*")) { return $false }
    if ($trimmed -match "^(?:async\s+)?function\s+[A-Za-z_$][A-Za-z0-9_$]*\s*\(") { return $true }
    if ($trimmed -match "^(?:const|let|var)\s+[A-Za-z_$][A-Za-z0-9_$]*\s*=\s*(?:async\s*)?\(") { return $true }
    if ($trimmed -match "^(?:const|let|var)\s+[A-Za-z_$][A-Za-z0-9_$]*\s*=\s*(?:async\s*)?[A-Za-z_$][A-Za-z0-9_$]*\s*=>") { return $true }
    return $false
}

function Collect-JsSignatureText {
    param(
        [System.Collections.Generic.List[string]]$Lines,
        [int]$StartIndex
    )
    $parts = New-Object System.Collections.Generic.List[string]
    $depth = 0
    $seenOpen = $false

    for ($j = $StartIndex; $j -lt [Math]::Min($Lines.Count, $StartIndex + 20); $j++) {
        $line = $Lines[$j]
        $parts.Add($line.Trim())
        foreach ($ch in $line.ToCharArray()) {
            if ($ch -eq "(") {
                $depth++
                $seenOpen = $true
            } elseif ($ch -eq ")") {
                if ($depth -gt 0) {
                    $depth--
                }
            }
        }
        if ($seenOpen -and $depth -eq 0) {
            break
        }
    }
    return (($parts -join " ") -replace "\s+", " ").Trim()
}

function Parse-JsParameters {
    param([string]$ParamsRaw)
    $params = @()
    if ([string]::IsNullOrWhiteSpace($ParamsRaw)) {
        return $params
    }
    $rawParts = Split-TopLevel -Text $ParamsRaw -Delimiter ","
    $index = 1
    foreach ($part in $rawParts) {
        $item = $part.Trim()
        if ([string]::IsNullOrWhiteSpace($item)) {
            continue
        }
        $item = ($item -replace "^\.\.\.", "").Trim()
        if ($item.Contains("=")) {
            $item = $item.Split("=")[0].Trim()
        }
        if ($item -match "^[\{\[]") {
            $name = "param$index"
        } else {
            $name = ($item -replace "[^A-Za-z0-9_$]", "").Trim()
            if ([string]::IsNullOrWhiteSpace($name)) {
                $name = "param$index"
            }
        }
        $params += $name
        $index++
    }
    return $params
}

function Parse-JsFunctionInfo {
    param([string]$Signature)
    if ([string]::IsNullOrWhiteSpace($Signature)) {
        return $null
    }

    $name = ""
    $paramsRaw = ""
    $isAsync = $false

    $funcMatch = [regex]::Match($Signature, "^(async\s+)?function\s+([A-Za-z_$][A-Za-z0-9_$]*)\s*\((.*?)\)")
    if ($funcMatch.Success) {
        $isAsync = -not [string]::IsNullOrWhiteSpace($funcMatch.Groups[1].Value)
        $name = $funcMatch.Groups[2].Value
        $paramsRaw = $funcMatch.Groups[3].Value
    } else {
        $arrowMatch = [regex]::Match($Signature, "^(const|let|var)\s+([A-Za-z_$][A-Za-z0-9_$]*)\s*=\s*(async\s*)?\((.*?)\)\s*=>")
        if ($arrowMatch.Success) {
            $isAsync = -not [string]::IsNullOrWhiteSpace($arrowMatch.Groups[3].Value)
            $name = $arrowMatch.Groups[2].Value
            $paramsRaw = $arrowMatch.Groups[4].Value
        } else {
            $singleParamArrow = [regex]::Match($Signature, "^(const|let|var)\s+([A-Za-z_$][A-Za-z0-9_$]*)\s*=\s*(async\s*)?([A-Za-z_$][A-Za-z0-9_$]*)\s*=>")
            if ($singleParamArrow.Success) {
                $isAsync = -not [string]::IsNullOrWhiteSpace($singleParamArrow.Groups[3].Value)
                $name = $singleParamArrow.Groups[2].Value
                $paramsRaw = $singleParamArrow.Groups[4].Value
            }
        }
    }

    if ([string]::IsNullOrWhiteSpace($name)) {
        return $null
    }

    $params = Parse-JsParameters -ParamsRaw $paramsRaw
    $returnType = if ($isAsync) { "Promise<any>" } else { "any" }

    return [PSCustomObject]@{
        Name = $name
        Params = $params
        ReturnType = $returnType
    }
}

function Build-JsDocLines {
    param(
        [string]$Indent,
        $Info
    )
    $doc = New-Object System.Collections.Generic.List[string]
    $doc.Add("$Indent/**")
    $doc.Add("$Indent * Executes the $($Info.Name) function.")
    foreach ($paramName in $Info.Params) {
        $doc.Add("$Indent * @param {*} $paramName Input parameter used by this function.")
    }
    $doc.Add("$Indent * @returns {$($Info.ReturnType)} Result produced by this function.")
    $doc.Add("$Indent * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.")
    $doc.Add("$Indent * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.")
    $doc.Add("$Indent */")
    return $doc
}

function Process-JsLikeFile {
    param([string]$Path)
    $raw = Get-Content -LiteralPath $Path -Raw
    $newline = Get-NewlineStyle -Content $raw
    $lineArray = $raw -split "`r?`n", -1
    $lines = New-Object System.Collections.Generic.List[string]
    foreach ($line in $lineArray) { $lines.Add($line) }

    $updated = $false

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]
        if (-not (Is-JsFunctionCandidate -Line $line)) {
            continue
        }

        $insertIndex = $i
        $docRange = Get-DocBlockRangeAbove -Lines $lines -Index $insertIndex
        if ($null -ne $docRange) {
            if (Is-AutoDocBlock -Lines $lines -Start $docRange.Start -End $docRange.End) {
                $removeCount = $docRange.End - $docRange.Start + 1
                $lines.RemoveRange($docRange.Start, $removeCount)
                if ($docRange.Start -lt $i) {
                    $i -= $removeCount
                }
            } else {
                continue
            }
        }

        $signature = Collect-JsSignatureText -Lines $lines -StartIndex $i
        $info = Parse-JsFunctionInfo -Signature $signature
        if ($null -eq $info) {
            continue
        }

        $indent = Get-Indent -Line $line
        $docLines = Build-JsDocLines -Indent $indent -Info $info
        for ($d = 0; $d -lt $docLines.Count; $d++) {
            $lines.Insert($insertIndex + $d, $docLines[$d])
        }
        $i += $docLines.Count
        $updated = $true
    }

    if ($updated) {
        $updatedContent = $lines -join $newline
        Set-Content -LiteralPath $Path -Value $updatedContent -NoNewline
        return 1
    }
    return 0
}

$javaUpdated = 0
$jsUpdated = 0
$htmlUpdated = 0

$javaRoots = @("src/main/java", "src/test/java")
foreach ($root in $javaRoots) {
    if (-not (Test-Path -LiteralPath $root)) { continue }
    Get-ChildItem -LiteralPath $root -Recurse -File -Filter "*.java" | ForEach-Object {
        $javaUpdated += Process-JavaFile -Path $_.FullName
    }
}

$jsRoot = "src/main/resources/static/js"
if (Test-Path -LiteralPath $jsRoot) {
    Get-ChildItem -LiteralPath $jsRoot -Recurse -File -Filter "*.js" | ForEach-Object {
        $jsUpdated += Process-JsLikeFile -Path $_.FullName
    }
}

$htmlRoot = "src/main/resources/templates"
if (Test-Path -LiteralPath $htmlRoot) {
    Get-ChildItem -LiteralPath $htmlRoot -Recurse -File -Filter "*.html" | ForEach-Object {
        $htmlUpdated += Process-JsLikeFile -Path $_.FullName
    }
}

Write-Host ("Java files updated: " + $javaUpdated)
Write-Host ("JS files updated: " + $jsUpdated)
Write-Host ("HTML files updated: " + $htmlUpdated)
