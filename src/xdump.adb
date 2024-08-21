with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Interfaces;

procedure Xdump is
   type Byte is new Interfaces.Unsigned_8;
   type Byte_Array is array (Ada.Directories.File_Size range <>) of Byte;
   subtype Hex_Byte_String is String (1 .. 2);
   package Byte_IO is new Ada.Direct_IO (Byte);

   --  Hex characters using Ada.Strings.Maps facilities.
   --  Use lower case; clients can convert to upper
   --  case as desired.
   Hex_Digit_Set : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Sequence => "0123456789abcdef");
   Hex_Chars : constant Ada.Strings.Maps.Character_Sequence :=
     Ada.Strings.Maps.To_Sequence (Hex_Digit_Set);

   --  Converts a Byte value into its hexadecimal two-digit string
   --  representation.
   function To_Hex (B : Byte) return Hex_Byte_String is
      High_Index : constant Integer := (Integer (B) / 16) + 1;
      Low_Index : constant Integer := (Integer (B) mod 16) + 1;
   begin
      return (Hex_Chars (High_Index), Hex_Chars (Low_Index));
   end To_Hex;

   type Character_Case is (Lower_Case, Upper_Case);

   type Dump_Configuration is record
      Char_Case : Character_Case := Upper_Case;
      Bytes_Per_Line : Positive := 16;
      Gap : Boolean := True; --  leave a gap between halves of dump line
   end record;

   --  Pad a string representation of an integer from the
   --  left with zeros.
   procedure Zero_Pad (S : String; Result : out String) is
   begin
      --  Easiest way to zero fill the offset from left:
      --  just replace the spaces with zeros.
      for I in S'Range loop
         Result (I) := S (I);
         if S (I) = ' ' then
            Result (I) := '0';
         end if;
      end loop;
   end Zero_Pad;

   --  Read all the bytes in the file.
   procedure Read_File (Name : String; Contents : out Byte_Array) is
      package SIO renames Ada.Streams.Stream_IO;

      Input_File   : SIO.File_Type;
      Input_Stream : SIO.Stream_Access;
      Index        : Ada.Directories.File_Size := 1;
      B            : Byte;
   begin
      SIO.Open (Input_File, SIO.In_File, Name);

      Input_Stream := SIO.Stream (Input_File);
      while not SIO.End_Of_File (Input_File) loop
         Byte'Read (Input_Stream, B);
         Contents (Index) := B;
         Index            := Index + 1;
      end loop;

      SIO.Close (Input_File);
   end Read_File;

   Config : Dump_Configuration;
   Size : Ada.Directories.File_Size;
   Name : Ada.Strings.Unbounded.Unbounded_String;
begin
   --Ada.Text_IO.Put_Line ("Max File_Size is " & Ada.Directories.File_Size'Last'Image);

   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line ("No input file");
      return;
   end if;

   Name := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Command_Line.Argument (1));
   declare
      File_Name : String := Ada.Strings.Unbounded.To_String (Name);
   begin
      if not Ada.Directories.Exists (File_Name) then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "File not found");
         return;
      end if;

      --  Now we are quite sure that the file exists, so get its size.
      Size := Ada.Directories.Size (File_Name);
   end;

   declare
      Data : Byte_Array (1 .. Size);
   begin
      Config.Char_Case := Lower_Case;

      Read_File (Ada.Strings.Unbounded.To_String (Name), Contents => Data);

      declare
         Counter : Integer;
         Bytes_Done : Integer;
         Counter_String : String (1 .. 10);
         Offset_String : String (1 .. 10);
         Chars : String (1 .. Config.Bytes_Per_Line);
      begin
         Counter := 0;
         Ada.Integer_Text_IO.Put (To => Counter_String, Item => Counter);
         Zero_Pad (Counter_String, Offset_String);
         Ada.Text_IO.Put (Offset_String & ": ");

         Bytes_Done := 0;
         for B of Data loop
            declare
               S : String := To_Hex (B);
               Ch : Character;
            begin
               if Config.Char_Case = Lower_Case then
                  S := Ada.Characters.Handling.To_Lower (Item => S);
               end if;
               Ada.Text_IO.Put (S & " ");

               Counter := Counter + 1;
               Bytes_Done := Bytes_Done + 1;

               if Bytes_Done = Config.Bytes_Per_Line / 2 and then Config.Gap then
                  Ada.Text_IO.Put (" ");
               end if;

               Ch := Character'Val (B);
               Chars (Bytes_Done) := (if Ada.Characters.Handling.Is_Graphic (Ch) then Ch else '.');

               if Counter mod Config.Bytes_Per_Line = 0 then
                  --  Output the printable characters for this line's bytes.
                  Ada.Text_IO.Put ("  " & Chars);
                  Ada.Text_IO.New_Line;
                  Bytes_Done := 0;  --  reset byte counter for a new line
                  Ada.Integer_Text_IO.Put (To => Counter_String, Item => Counter);
                  Zero_Pad (Counter_String, Offset_String);
                  Ada.Text_IO.Put (Offset_String & ": ");
               end if;
            end;
         end loop;
         --  We're out of data, output some extra space to fill
         --  the hole, then output the printable characters for the
         --  last incomplete line.
         for I in Bytes_Done + 1 .. Config.Bytes_Per_Line loop
            Ada.Text_IO.Put ("   ");
         end loop;
         Ada.Text_IO.Put ("  ");
         for I in Chars'First .. Bytes_Done loop
            Ada.Text_IO.Put (Chars (I));
         end loop;
      end;

      Ada.Text_IO.New_Line;  --  one final newline to finish the display
   exception
      when Name_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "File not found");
   end;
end Xdump;
