# tine
TINE (This is not EFI) - A fake-(U)EFI boot loader for pure BIOS environments

A fake-(U)EFI boot loader, which loads a PE64 EFI application image on the 1 MB boundary, but this boot loader doesn't emulate the (U)EFI API interface, because it's designed for kernel images, which can differ from what they are loaded, whether from a real (U)EFI or fake (U)EFI image boot loader, for to have a single kernel image file for both BIOS/CSM and (U)EFI worlds, for to simplify the operating system development on x86-64 for to support both worlds. 

When a PE64 EFI application image is loaded from this fake-(U)EFI boot loader, then the RAX register contains 0xbabeface and the RBX register contains a pointer to a multiboot-based data info structure (with informations to memory map, boot drive, etc.

It supports FAT12, FAT16 and FAT32 file systems with auto-detection, where the VBR boot sector must be loaded at 0x07c0:0x0000 / 0x0000:0x7c00 since this boot loader parses the BPB data from this memory place.

You do need my own assembler SASM for to build this source code. For to get the current stable SASM binary, write me a mail at benjamin[at]rosseaux[dot]de together with your used development operating system, so that I can compile my assembler for your OS.    

    ****************************************************************************************
    **
    ** Copyright (C) 2015, Benjamin 'BeRo' Rosseaux ( benjamin[at]rosseaux[dot]de )
    ** All rights reserved.
    ** 
    ** Redistribution and use in source and binary forms, with or without
    ** modification, are permitted provided that the following conditions are met:
    ** 
    ** 1. Redistributions of source code must retain the above copyright notice, this
    **    list of conditions and the following disclaimer.
    ** 2. Redistributions in binary form must reproduce the above copyright notice,
    **    this list of conditions and the following disclaimer in the documentation
    **    and/or other materials provided with the distribution.
    ** 3. Neither the name of the copyright holders nor the names of its contributors 
    **    may be used to endorse or promote products derived from this software without 
    **    specific prior written permission.
    ** 
    ** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    ** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    ** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ** ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    ** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    ** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
    ** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    ** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    ** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    ** 
    ** The views and conclusions contained in the software and documentation are those
    ** of the authors and should not be interpreted as representing official policies,
    ** either expressed or implied, of the author.
    **
    ***************************************************************************************
    
    
