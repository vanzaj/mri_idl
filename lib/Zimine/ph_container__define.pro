;+
; NAME: ph_Container
;
; Extended IDL_Container
; Contained ph_BaseObj objects can be retrived using object names
;
; Cleanup will automatically remove ph_BaseObj objects that have _destroy EQ 0
; (i.e. they will NOT be automatically destroyed)
;
; METHODS:
;   obj = obj->FindByName(searchStr, /RegExp, /Fold_Case, Count=count)
;   obj = obj->Get(Name=search) ; extended original method
;   obj->Remove, Name=search    ; extended original method
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;   2008-04-30 ivz: Original
;-
; Modified version of catcontainer__define.pro
; Catalyst Library from http://www.dfanning.com
;******************************************************************************************;
;  Copyright (c) 2008, jointly by Fanning Software Consulting, Inc.                        ;
;  and Burridge Computing. All rights reserved.                                            ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       ;
;        nor the names of its contributors may be used to endorse or promote products      ;
;        derived from this software without specific prior written permission.             ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. AND BURRIDGE COMPUTING   ;
;  ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ;
;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          ;
;  DISCLAIMED. IN NO EVENT SHALL FANNING SOFTWARE CONSULTING, INC. OR BURRIDGE COMPUTING   ;
;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ;
;  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;

function ph_Container::GetNames, NoEmpty=noempty
  compile_opt hidden
  ON_ERROR, 2
  
  list = self->IDL_Container::Get(/all, Count=count)
  if count EQ 0 then return, ''

  names = StrArr(count)
  for i=0L, count-1 do begin
    if obj_valid_isa(list[i], 'ph_BaseObj') then $
      names[i] = list[i] -> _name()
  endfor

  ;; remove empty names (like '', '  ')
  if Keyword_Set(noempty) then begin
    wh = where(strlen(strtrim(names,2)) GT 0, count)
    if count gt 0 then names = names[wh]
  endif

  return, names
end

function ph_Container::FindByName, search, RegExp=regexp, $
      Fold_Case=fold_case, Count=count

  compile_opt hidden
  ON_ERROR, 2
  null = obj_new()
  count=0
  ;; empty container
  if ( self->Count() eq 0) then return, null

  ;; search only ph_BaseObj objects 
  list = self->IDL_Container::Get(/all, ISA='ph_BaseObj', Count=count)
  if count EQ 0 then return, null

  names = strarr(count)
  for i=0L, count-1 do $
    names[i] = list[i] -> _name()

  if Keyword_Set(regexp) then $
    match = stregex(names, search, /boolean, fold_case=fold_case) $
  else match = strmatch(names, search, fold_case=fold_case) 

  wh = where(match, count)
  if count eq 0 then return, null

  count=1
  return, list[wh[0]]
end

function ph_Container::Get, Name=search, RegExp=regexp, Fold_Case=fold_case, $ 
      ALL=all, ISA=isa, Position=position, Count=count

  compile_opt hidden
  ON_ERROR, 2

  null = OBJ_NEW ()
  count = 0

  ;; empty container
  if ( self->Count() eq 0 ) then return, null

  ;; use default Get() if there is no name
  if n_elements(search) eq 0 then begin
    ret = self->IDL_Container::Get(Position=position, ALL=all, ISA=isa, Count=count)
    ;; change -1 to null if nothing found
    if count EQ 0 then ret=null
    return, ret
  endif

  ret = self->FindByName(search, RegExp=regexp, Fold_Case=fold_case, Count=count)
  return, ret
END

PRO ph_Container::Remove, obj, All=all, Position=position, $
      Name=search, RegExp=regexp, Fold_Case=fold_case

  compile_opt hidden
  ON_ERROR, 2

  ;; remove by name
  if N_Elements(search) gt 0 then begin
    ref = self->FindByName(search, RegExp=regexp, Fold_Case=fold_case, Count=count)
    if (count GT 0) then begin
      self->IDL_Container::Remove, ref
      return
    endif
  endif

  ; default remove
  if n_elements(obj) gt 0 then begin
    self->IDL_Container::Remove, obj, all=all, position=position
  endif
END

PRO ph_Container::Cleanup 

  compile_opt hidden
  list = self->IDL_Container::Get(/all, isa='ph_BaseObj', count=cn)
  for i=0L, cn-1 do begin
    if ~obj_valid(list[i]) then continue
    list[i] -> GetProperty, destroy=destroy
    if (destroy eq 0) then self->remove, list[i]
  endfor
  self->IDL_Container::Cleanup
END

FUNCTION ph_Container::Init

  compile_opt hidden
  return, self->IDL_Container::Init()
END

PRO ph_Container__DEFINE
  compile_opt hidden
  class = { ph_Container,          $ 
            INHERITS IDL_CONTAINER $
  }
END

