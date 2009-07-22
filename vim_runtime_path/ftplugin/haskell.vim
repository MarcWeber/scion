if exists('g:dont_load_haskell_scion_interface_simple')
  finish
endif

" r = scion result with error locations
" func : either setqflist or setloclist
fun! ScionResultToErrorList(action, func, r)
  let compilationResult = has_key(a:r, 'compilationResult') ? a:r['compilationResult'] : a:r
  let g:foo = compilationResult
  let qflist = compilationResult['compilationErrors'] + compilationResult['compilationWarnings']

  " for debugging
  let g:scion_qf_list = qflist
  if has_key(a:r, 'inProject')
    let inProj = "inProject : ". a:r['inProject']
  else
    let inProj = ""
  endif
  if (has_key(a:r,'compilationSucceeded') && a:r['compilationSucceeded']) 
        \ || (!has_key(a:r, 'compilationSucceeded') && len(qflist) == 0)
    return printf(a:action." success. ".inProj." compilationTime: %s", compilationResult['compilationTime'])
  else
    call call(a:func, [qflist])
    return printf(a:action." there are errors, ".inProj." compilationTime: %s", compilationResult['compilationTime'])
  endif
endfun

" very simple user interface to expose scion functionality
" I'll implement a better interface in tovl.
" (http://github.com/MarcWeber/theonevimlib)

fun! s:BackgroundTypecheckFile(...)
  " no file given defaults to current buffer
  let file = a:0 > 0 ? a:1 : expand('%:p')
  let r =  haskellcomplete#EvalScion(1,{'method' : 'cmdBackgroundTypecheckFile', 'file' : file})
  echo ScionResultToErrorList('file check', 'setqflist', r)
endf

fun! s:OpenCabalProject(method, ...)
  echo haskellcomplete#EvalScion(1,a:method
    \  ,{'root-dir' : getcwd()
    \   ,'dist-dir' : a:1
    \   ,'extra-args' : a:000[1:] }
    \ )
endf

fun! s:LoadComponentCompletion(A,L,P)
  let beforeC= a:L[:a:P-1]
  let word = matchstr(beforeC, '\zs\S*$')

  let result = []
  for item in haskellcomplete#EvalScion(1,'list-cabal-components',{'cabal-file': haskellcomplete#CabalFile()})
    if has_key(item, 'library')
      call add(result, 'library') " there can only be one
    elseif has_key(item, 'executable')
      call add(result, 'executable:'. item['executable'])
    else
      " component type File will never be returned ?
      throw "unexpected item ".string(item)
    endif
  endfor
  return result
endf


" intentionally suffixing commands by "Scion"
" This way you have less typing. You can still get a list of Scion commands by
" :*Scion<c-d>

" ===== you don't need any project for these:  =============
command! -buffer ConnectionInfoScion
  \ echo haskellcomplete#EvalScion(1,'connection-info',{})

" list supported languages
" TODO
command! -buffer ListSupportedLanguagesScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdListSupportedLanguages'})

" list supported pragmas
" TODO
command! -buffer ListSupportedPragmasScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdListSupportedPragmas'})

" list supported flags
" TODO
command! -buffer ListSupportedFlagsScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdListSupportedFlags'})

" ===== loading a cabal project: ============================

" assuming pwd is current cabal directory containing the .cabal file 
" optional argument specifies the cabal build (dist) directory
command! -buffer -nargs=* -complete=file OpenCabalProjectScion
  \ call s:OpenCabalProject('open-cabal-project',<f-args>)
command! -buffer -nargs=* -complete=file ConfigureCabalProjectScion
  \ call s:OpenCabalProject('configure-cabal-project', <f-args>)

" arg either "library" or "executable:name"
" TODO
command! -buffer -nargs=1 -complete=customlist,s:LoadComponentCompletion
  \ LoadComponentScion
  \ echo haskellcomplete#LoadComponent(haskellcomplete#compToV(<f-args>))
  " \ echo ScionResultToErrorList('load component finished: ','setqflist',haskellcomplete#EvalScion(1,'load-component', { 'component' : haskellcomplete#compToV(<f-args>)}))

" list exposed 
" TODO
command! -buffer ListExposedModulesScion
  \ echo haskellcomplete#EvalScion(1, {'method' : 'cmdListExposedModules'})
" TODO
command! -buffer -nargs=* -complete=file BackgroundTypecheckFileScion
  \ call s:BackgroundTypecheckFile(<f-args>)
" TODO
command! -buffer ThingAtPointScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdThingAtPoint', 'file' : expand('%:p'), 'line' : line('.').'', 'col' : col('.').''})
" TODO
command! -buffer ThingAtPointExportedByHackScion
  \ echo filter(
      \ split(haskellcomplete#EvalScion(1,{'method' : 'cmdThingAtPointMoreInfo'
        \, 'file' : expand('%:p')
        \, 'line' : line('.').'', 'col' : col('.').''})['Just'],"\n")
      \ , 'v:val =~ '.string(expand('<cword>').' ='))[0]

" TODO
command! -buffer ListRdrNamesInScopeScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdListRdrNamesInScope'})

" TODO
command! -buffer ListCabalTargetsScion
  \ echo haskellcomplete#EvalScion(1,{'method' : 'cmdListCabalTargets'})
