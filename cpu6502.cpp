#include "../Include/cpu6502.hpp"

cpu6502::cpu6502()
{
    instructions =
    {
		{ "BRK", &BRK, &IMM, 7 }, { "ORA", &ORA, &IZX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "ORA", &ORA, &ZPA, 3 }, { "ASL", &ASL, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PHP", &PHP, &IMP, 3 }, { "ORA", &ORA, &IMM, 2 }, { "ASL", &ASL, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ABS, 4 }, { "ASL", &ASL, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BPL", &BPL, &REL, 2 }, { "ORA", &ORA, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ZPX, 4 }, { "ASL", &ASL, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLC", &CLC, &IMP, 2 }, { "ORA", &ORA, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ABX, 4 }, { "ASL", &ASL, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "JSR", &JSR, &ABS, 6 }, { "AND", &AND, &IZX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "BIT", &BIT, &ZPA, 3 }, { "AND", &AND, &ZPA, 3 }, { "ROL", &ROL, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PLP", &PLP, &IMP, 4 }, { "AND", &AND, &IMM, 2 }, { "ROL", &ROL, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "BIT", &BIT, &ABS, 4 }, { "AND", &AND, &ABS, 4 }, { "ROL", &ROL, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BMI", &BMI, &REL, 2 }, { "AND", &AND, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "AND", &AND, &ZPX, 4 }, { "ROL", &ROL, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SEC", &SEC, &IMP, 2 }, { "AND", &AND, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "AND", &AND, &ABX, 4 }, { "ROL", &ROL, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "RTI", &RTI, &IMP, 6 }, { "EOR", &EOR, &IZX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "EOR", &EOR, &ZPA, 3 }, { "LSR", &LSR, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PHA", &PHA, &IMP, 3 }, { "EOR", &EOR, &IMM, 2 }, { "LSR", &LSR, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "JMP", &JMP, &ABS, 3 }, { "EOR", &EOR, &ABS, 4 }, { "LSR", &LSR, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BVC", &BVC, &REL, 2 }, { "EOR", &EOR, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "EOR", &EOR, &ZPX, 4 }, { "LSR", &LSR, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLI", &CLI, &IMP, 2 }, { "EOR", &EOR, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "EOR", &EOR, &ABX, 4 }, { "LSR", &LSR, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "RTS", &RTS, &IMP, 6 }, { "ADC", &ADC, &IZX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "ADC", &ADC, &ZPA, 3 }, { "ROR", &ROR, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PLA", &PLA, &IMP, 4 }, { "ADC", &ADC, &IMM, 2 }, { "ROR", &ROR, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "JMP", &JMP, &IND, 5 }, { "ADC", &ADC, &ABS, 4 }, { "ROR", &ROR, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BVS", &BVS, &REL, 2 }, { "ADC", &ADC, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "ADC", &ADC, &ZPX, 4 }, { "ROR", &ROR, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SEI", &SEI, &IMP, 2 }, { "ADC", &ADC, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "ADC", &ADC, &ABX, 4 }, { "ROR", &ROR, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "###", &NOP, &IMP, 2 }, { "STA", &STA, &IZX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 6 }, { "STY", &STY, &ZPA, 3 }, { "STA", &STA, &ZPA, 3 }, { "STX", &STX, &ZPA, 3 }, { "###", &XXX, &IMP, 3 }, { "DEY", &DEY, &IMP, 2 }, { "###", &NOP, &IMP, 2 }, { "TXA", &TXA, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "STY", &STY, &ABS, 4 }, { "STA", &STA, &ABS, 4 }, { "STX", &STX, &ABS, 4 }, { "###", &XXX, &IMP, 4 },
		{ "BCC", &BCC, &REL, 2 }, { "STA", &STA, &IZY, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 6 }, { "STY", &STY, &ZPX, 4 }, { "STA", &STA, &ZPX, 4 }, { "STX", &STX, &ZPY, 4 }, { "###", &XXX, &IMP, 4 }, { "TYA", &TYA, &IMP, 2 }, { "STA", &STA, &ABY, 5 }, { "TXS", &TXS, &IMP, 2 }, { "###", &XXX, &IMP, 5 }, { "###", &NOP, &IMP, 5 }, { "STA", &STA, &ABX, 5 }, { "###", &XXX, &IMP, 5 }, { "###", &XXX, &IMP, 5 },
		{ "LDY", &LDY, &IMM, 2 }, { "LDA", &LDA, &IZX, 6 }, { "LDX", &LDX, &IMM, 2 }, { "###", &XXX, &IMP, 6 }, { "LDY", &LDY, &ZPA, 3 }, { "LDA", &LDA, &ZPA, 3 }, { "LDX", &LDX, &ZPA, 3 }, { "###", &XXX, &IMP, 3 }, { "TAY", &TAY, &IMP, 2 }, { "LDA", &LDA, &IMM, 2 }, { "TAX", &TAX, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "LDY", &LDY, &ABS, 4 }, { "LDA", &LDA, &ABS, 4 }, { "LDX", &LDX, &ABS, 4 }, { "###", &XXX, &IMP, 4 },
		{ "BCS", &BCS, &REL, 2 }, { "LDA", &LDA, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 5 }, { "LDY", &LDY, &ZPX, 4 }, { "LDA", &LDA, &ZPX, 4 }, { "LDX", &LDX, &ZPY, 4 }, { "###", &XXX, &IMP, 4 }, { "CLV", &CLV, &IMP, 2 }, { "LDA", &LDA, &ABY, 4 }, { "TSX", &TSX, &IMP, 2 }, { "###", &XXX, &IMP, 4 }, { "LDY", &LDY, &ABX, 4 }, { "LDA", &LDA, &ABX, 4 }, { "LDX", &LDX, &ABY, 4 }, { "###", &XXX, &IMP, 4 },
		{ "CPY", &CPY, &IMM, 2 }, { "CMP", &CMP, &IZX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "CPY", &CPY, &ZPA, 3 }, { "CMP", &CMP, &ZPA, 3 }, { "DEC", &DEC, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "IZY", &IZY, &IMP, 2 }, { "CMP", &CMP, &IMM, 2 }, { "DEX", &DEX, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "CPY", &CPY, &ABS, 4 }, { "CMP", &CMP, &ABS, 4 }, { "DEC", &DEC, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BNE", &BNE, &REL, 2 }, { "CMP", &CMP, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "CMP", &CMP, &ZPX, 4 }, { "DEC", &DEC, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLD", &CLD, &IMP, 2 }, { "CMP", &CMP, &ABY, 4 }, { "NOP", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "CMP", &CMP, &ABX, 4 }, { "DEC", &DEC, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "CPX", &CPX, &IMM, 2 }, { "SBC", &SBC, &IZX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "CPX", &CPX, &ZPA, 3 }, { "SBC", &SBC, &ZPA, 3 }, { "INC", &INC, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "IZX", &IZX, &IMP, 2 }, { "SBC", &SBC, &IMM, 2 }, { "NOP", &NOP, &IMP, 2 }, { "###", &SBC, &IMP, 2 }, { "CPX", &CPX, &ABS, 4 }, { "SBC", &SBC, &ABS, 4 }, { "INC", &INC, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BEQ", &BEQ, &REL, 2 }, { "SBC", &SBC, &IZY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "SBC", &SBC, &ZPX, 4 }, { "INC", &INC, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SED", &SED, &IMP, 2 }, { "SBC", &SBC, &ABY, 4 }, { "NOP", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "SBC", &SBC, &ABX, 4 }, { "INC", &INC, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
    };
}

void cpu6502::reset()
{
	stack_ptr = 0;
	program_counter = 0;
}

void cpu6502::clock()
{
	// Wait until the last instruction has been executed
	if (cycles == 0)
	{
		// Read an opcode
		opcode = read(program_counter);

		// Move on to the next opcode
		program_counter++;

		// Set a number of cycles to execute
		cycles = instructions[opcode].cycles;

		// Set to 1 as it always must be 1
		set_flag(flag_1, true);

		// Perform an addressing mode
		bool add_cycle_1 = (this->*instructions[opcode].addr_mode)();

		// Execute an instruction itself
		bool add_cycle_2 = (this->*instructions[opcode].opcode)();

		set_flag(flag_1, true);

		// Add an additional cycle if it's needed
		if (add_cycle_1 && add_cycle_2)
			cycles++;
	}

	cycles--;
}

void cpu6502::connect_bus(Bus* bus)
{
	this->bus = bus;
}

void cpu6502::write(uint16_t addr, uint8_t value)
{
	bus->write(addr, value);
}

uint8_t cpu6502::read(uint16_t addr)
{
	return bus->read(addr);
}

void cpu6502::push_stack(uint8_t value)
{
	write(0x100 + (uint16_t)stack_ptr, value);
	stack_ptr--;
}

void cpu6502::memorize()
{
	// We want to memorize data for every addressing mode
	// except the IMPlied one because there is nothing
	// to memorize

	if (instructions[opcode].addr_mode != &IMP)
		memory = read(abs_addr);
}

/* Immediate AM
| The data is supplied as part of the instruction,
| so it stores the address of the next byte
| (of the argument of the instruction)
*/
bool cpu6502::IMM()
{
	abs_addr = program_counter++;
	return false;
}

/* Absolute AM
| Here we specify the full 16-bit address of the data to use.
*/
bool cpu6502::ABS()
{
	// Read 8 rightmost bits
	abs_addr_1 = read(program_counter++);

	// Read 8 leftmost bits
	abs_addr_0 = read(program_counter++);

	return false;
}

/* Pages
| Note that we use 2KB of RAM connected to the bus,
| 2KB = 256 * 256 bytes so the idea is to split
| the RAM into 256 pages where each page has 256 bytes in it,
| since that each address is 16 bit where first 8 bits are the
| number of a page and the next 8 bits are the offset in that page
| to the data itself. Example of the address:
|                       $43F4
|                        --==
|                        ^  ^
|                     page  offset
*/

/* Zero Page AM
| Instructions with that mode take data from the
| 0'th page and the instruction only provides an
| offset on that (0'th) page
*/
bool cpu6502::ZPA()
{
	// Set first byte to 0 and set
	// last byte to the 1 byte address that
	// we read on from the RAM so we get
	// something like that: $0053. So as you can see
	// we access the 0'th page with the offset of $53

	abs_addr_0 = 0x00;
	abs_addr_1 = read(program_counter++);

	return false;
}

/* Zero Page X AM
| The same thing as ZPA but also use X register to offset the offset
*/
bool cpu6502::ZPX()
{
	abs_addr_0 = 0x00;
	abs_addr_1 = read(program_counter++) + x;

	return false;
}

/* Zero Page Y AM
| The same thing as ZPA but also use Y register to offset the offset
*/
bool cpu6502::ZPY()
{
	abs_addr_0 = 0x00;
	abs_addr_1 = read(program_counter++) + y;

	return false;
}

/* Absolute X AM
| Here we specify the full 16-bit address of the data to use
| and offset it by the X register
*/
bool cpu6502::ABX()
{
	// Read 8 rightmost bits
	abs_addr_1 = read(program_counter++);

	// Read 8 leftmost bits
	abs_addr_0 = read(program_counter++);

	uint8_t old_left = abs_addr_0;

	// Offset by the register value
	abs_addr += x;

	// VERY IMPORTANT! After offseting the absolute address
	// by the register value the page crossing may occur
	// so if we cross the page then add 1 more clock cycle
	// to the instruction
	
	// We can identify such situation by comparing the
	// first byte of the new address and the first byte
	// of the old address
	if (abs_addr_0 != old_left)
		return true;

	return false;
}

/* Absolute Y AM
| Here we specify the full 16-bit address of the data to use
| and offset it by the Y register
*/
bool cpu6502::ABY()
{
	// The same as for X register but here we use Y register

	abs_addr_1 = read(program_counter++);
	abs_addr_0 = read(program_counter++);

	uint8_t old_page = abs_addr_0;
	abs_addr += y;

	if (abs_addr_0 != old_page)
		return true;

	return false;
}

/* Implied AM
| simply saves a value from
| an accumulator into a memory
*/
bool cpu6502::IMP()
{
	memory = accumulator;
	return false;
}

/* Relative AM
| Only applied to the branching instructions (e.g. JMP), so it must
| set relative address to jump from the current location in the program
| to another
*/
bool cpu6502::REL()
{
	rel_addr = read(program_counter++);

	if (rel_addr & 0b10000000)
	{
		// If the first bit is set to 1 then we have an unsigned type
		// but we need to treat it like a signed type because
		// we want to be able to jump backwards
		rel_addr |= 0xFF00;
	}

	return false;
}

/* Indirect Zero Page AM with the X register
| Takes a value from the RAM and uses it as a pointer to the data,
| also offsets the pointer by the X register
*/
bool cpu6502::IZX()
{
	uint16_t ptr = read(program_counter++);

	abs_addr_1 = read((ptr + x + 0) & 0x00FF);
	abs_addr_0 = read((ptr + x + 1) & 0x00FF);

	return false;
}

/* Indirect Zero Page AM with the Y register
| Takes a value from the RAM and uses it as a pointer to the data,
| also offsets not the pointer but the final address itself by the Y register.
| Notice that here we also check for a page crossing.
*/
bool cpu6502::IZY()
{
	uint16_t ptr = read(program_counter++);

	abs_addr_1 = read((ptr + 0) & 0x00FF);
	abs_addr_0 = read((ptr + 1) & 0x00FF);

	uint8_t old_page = abs_addr_0;
	abs_addr += y;

	if (abs_addr_0 != old_page)
	{
		// The page crossing has occured
		return true;
	}

	return false;
}

/* Indirect AM
| The argument of the executed instruction is an address
| of the address of the data that must be used
*/
bool cpu6502::IND()
{
	// Read right and left byte of the pointer
	uint16_t ptr_right = read(program_counter++);
	uint16_t ptr_left = read(program_counter++);

	// Construct the pointer
	uint16_t ptr = (ptr_left << 8) | ptr_right;

	// One crucial moment: if the ptr occurs to be $FF then
	// we read the high byte of the abs_addr at ($FF + $1)
	// so we cross the current page but there is a hardware
	// bug in the 6502 CPU that doesn't allow us to move to
	// the next page if there is a page crossing
	// (i.e. ptr_right is $FF)

	// All found bugs: https://www.nesdev.org/6502bugs.txt

	abs_addr_1 = read(ptr);

	// Simulating the "correct" behaviour

	if (ptr_right == 0x00FF) // ignore page crossing
		abs_addr_0 = read(ptr & 0xFF00);
	else
		abs_addr_0 = read(ptr + 1);

	return false;
}

void cpu6502::set_flag(uint8_t flag, bool enable)
{
	// Note that flag is a value from the Flags enum
	// because there we have precalculated an offset
	// for each bit

	if (enable)
	{
		// Example: 00001000 | 01000000 = 01001000
		// so we've enabled the second bit

		status |= flag;
	}
	else
	{
		// Example: status=01001000, flag=01000000,
		// ~flag = ~01000000 = 10111111,
		// status & ~flag = 01001000 & 10111111 = 00001000
		// so we've turned off the second bit

		status &= ~flag;
	}
}

bool cpu6502::get_flag(uint8_t flag) const
{
	return (status & flag) != 0;
}

/* ADd with a Carry
| Simly performs addition. However, it is not as easy as it sounds:
| you need to keep in mind that you can work with signed and unsigned numbers
| and of course they have their own numeric borders, so this operation
| changes negative (n), overflow (v) flags and zero (z) flags
*/
bool cpu6502::ADC()
{
	/* Situations when overflow is occured:
	* positive + positive = negative
	* negative + negative = positive */

	// There is a formula that handles that:
	// v = (accumulator ^ result) & ~(accumulator ^ memory)

	memorize();

	uint16_t res = (uint16_t)memory + (uint16_t)accumulator;
	if (get_flag(flag_c)) res++;

	// Overflow
	bool v = ((uint16_t)accumulator ^ res) & ~((uint16_t)accumulator ^ memory);
	set_flag(flag_v, v & 0b10000000);

	// Store the result (taking last 1 byte because the result is actually 1 byte in size)
	accumulator = res & 0x00FF;

	// Check for a 0 result
	set_flag(flag_z, accumulator == 0);

	// Set if there is something to carry because if there is
	// we will accumulate it on the next call of the ADC instruction
	set_flag(flag_c, res > 255);

	// If the first bit is set to 1 then we've got a negative number
	set_flag(flag_n, res & 0b10000000);

	return true;
}

/* Binary AND
| Performs A = A & memory
| Sets Z and N flags
*/
bool cpu6502::AND()
{
	memorize();
	accumulator &= memory;
	
	set_flag(flag_z, accumulator == 0);
	set_flag(flag_n, accumulator & 0b10000000); // 7th bit

	// requires additional clock cycle
	return true;
}

/* Arithmetic Shift Left
| Performs M << 1 and changes carry (c), zero (z) and negative (n) flags
| and stores the result into accumulator or memory
*/
bool cpu6502::ASL()
{
	memorize();
	uint16_t res_ext = (uint16_t)memory << 1;

	set_flag(flag_c, memory & 0b10000000);
	set_flag(flag_n, res_ext & 0b10000000);

	uint8_t res = res_ext & 0x00FF;

	// Only applies to the accumulator
	set_flag(flag_z, res == 0);

	// We write to the accumulator if the addressing mode is implied
	// otherwise we write to the memory

	if (instructions[opcode].addr_mode == &IMP)
		accumulator = res;
	else
		write(abs_addr, res);

	return false;
}

/* Everything you need to know to implement branch instructions
*  (notice that all of them are the same, the only thing
*   that changes is the flag checker):
*  use this https://www.nesdev.org/wiki/Instruction_reference and
*  https://www.nesdev.org/obelisk-6502-guide/reference.html
*  because some instructions' descriptions are not clear on the first link
*  and better described on the second link, however some of the instructions
*  are corrupted on the second link (e.g. ASL flags' description)
*/

/* Conditional Branching Base
| Simply hides all repetitive code for every branching instruction
*/
void cpu6502::cond_branch_base(uint8_t flag, bool value)
{
	if (get_flag(flag) == value)
	{
		// We always add one clock cycle
		cycles++;

		// Calculate a new location
		abs_addr = program_counter + rel_addr;

		// Check for a page boundary crossing
		if (abs_addr_0 != (program_counter & 0xFF00))
			cycles++;

		// Move to the new location
		program_counter = abs_addr;
	}
}

/* Branch if Carry Clear
| If the carry flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BCC()
{
	cond_branch_base(flag_c, false);
	return false;
}

/* Branch if Carry Set
| If the carry flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BCS()
{
	cond_branch_base(flag_c, true);
	return false;
}

/* Branch if Equal
| If the zero flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BEQ()
{
	cond_branch_base(flag_z, true);
	return false;
}

/* Bit Test
| This instruction doesn't change the memory or the registers,
| it changes Z, V and N flags
*/
bool cpu6502::BIT()
{
	memorize();

	// Storing the result of BAND into the accumulator
	accumulator &= memory;

	set_flag(flag_z, accumulator == 0);
	set_flag(flag_v, memory & 0b01000000); // 6th bit
	set_flag(flag_n, memory & 0b10000000); // 7th bit

	return false;
}

/* Branch if Minus
| If the negative flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BMI()
{
	cond_branch_base(flag_n, true);
	return false;
}

/* Branch if Not Equal
| If the zero flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BNE()
{
	cond_branch_base(flag_z, false);
	return false;
}

/* Branch if Positive
| If the negative flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BPL()
{
	cond_branch_base(flag_n, false);
	return false;
}

bool cpu6502::BRK()
{
	return false;
}

/* Branch if Overflow Clear
| If the overflow flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BVC()
{
	cond_branch_base(flag_v, false);
	return false;
}

/* Branch if Overflow Set
| If the overflow flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool cpu6502::BVS()
{
	cond_branch_base(flag_v, true);
	return false;
}

/* Clear Carry Flag
| Sets the carry flag to 0
*/
bool cpu6502::CLC()
{
	set_flag(flag_c, false);
	return false;
}

/* Clear Decimal Mode
| Sets the decimal mode flag to 0
*/
bool cpu6502::CLD()
{
	set_flag(flag_d, false);
	return false;
}

/* Clear Interrupt Disable
| Sets the interrupt disable flag to 0
*/
bool cpu6502::CLI()
{
	set_flag(flag_i, false);
	return false;
}

/* Clear Overflow Flag
| Sets the overflow flag to 0
*/
bool cpu6502::CLV()
{
	set_flag(flag_v, false);
	return false;
}

/* CoMPare A
| Simply compares values of the accumulator and of the memory
| by performing A - M and then it modifies carry, zero and negative flags,
| notice that it does not change a value of anything
*/
bool cpu6502::CMP()
{
	memorize();
	uint16_t res = (uint16_t)accumulator - (uint16_t)memory;

	set_flag(flag_c, accumulator >= memory);
	set_flag(flag_z, accumulator == memory);
	set_flag(flag_n, res & 0b10000000);

	return true;
}

/* CoMPare X
| The same as CMP but here we use X register instead of the accumulator
*/
bool cpu6502::CPX()
{
	memorize();
	uint16_t res = (uint16_t)x - (uint16_t)memory;

	set_flag(flag_c, x >= memory);
	set_flag(flag_z, x == memory);
	set_flag(flag_n, res & 0b10000000);

	return true;
}

/* CoMPare Y
| The same as CMP but here we use Y register instead of the accumulator
*/
bool cpu6502::CPY()
{
	memorize();

	set_flag(flag_c, y >= memory);
	set_flag(flag_z, y == memory);
	set_flag(flag_n, ((uint16_t)y - (uint16_t)memory) & 0b10000000);

	return true;
}

/* DECrement memory
| Performs M = M - 1 and there is no equivalent for the accumulator,
| only affects Z and N flags
*/
bool cpu6502::DEC()
{
	memorize();

	uint8_t res = memory - 1;
	write(abs_addr, res);

	set_flag(flag_z, res == 0);
	set_flag(flag_n, res & 0b10000000);

	return false;
}

/* DEcrement X
| Decrements a value of the X register and sets Z and N flags
*/
bool cpu6502::DEX()
{
	x--;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* DEcrement Y
| Decrements a value of the Y register and sets Z and N flags
*/
bool cpu6502::DEY()
{
	y--;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

/* Exclusive OR
| Performs A = A ^ M and modifies the Z and N flags
*/
bool cpu6502::EOR()
{
	memorize();
	accumulator ^= memory;

	set_flag(flag_z, accumulator == 0);
	set_flag(flag_n, accumulator & 0b10000000);

	return true;
}

/* INCrement memory
| Performs M = M + 1 and notice there is no alternative for the accumulator,
| also modifies Z and N flags
*/
bool cpu6502::INC()
{
	memorize();

	uint8_t res = memory + 1;
	write(abs_addr, res);

	set_flag(flag_z, res == 0);
	set_flag(flag_n, res & 0b10000000);

	return false;
}

bool cpu6502::XXX()
{
	// Nothing to do
	return false;
}

/* JuMP
| Jumping to the specified address
*/
bool cpu6502::JMP()
{
	// We have precalculated the address in the addressing mode
	program_counter = abs_addr; 
	return false;
}

/* Jump to SubRoutine
| Pushes the pointer to the previous instruction to the stack and
| jumps to the new address
*/
bool cpu6502::JSR()
{
	// We decrement it because the return address on the stack
	// points 1 byte before the start of the next instruction
	program_counter--;

	push_stack((program_counter << 8) & 0x00FF);
	push_stack(program_counter & 0x00FF);

	program_counter = abs_addr;

	return false;
}

/* LoaD A
| Loads the memory content into the accumulator,
| sets zero and negative flags
*/
bool cpu6502::LDA()
{
	memorize();
	accumulator = memory;

	set_flag(flag_z, accumulator == 0);
	set_flag(flag_n, accumulator & 0b10000000);

	return true;
}

/* LoaD X
| Loads the memory content into the X register,
| sets zero and negative flags
*/
bool cpu6502::LDX()
{
	memorize();
	x = memory;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return true;
}

/* LoaD Y
| Loads the memory content into the Y register,
| sets zero and negative flags
*/
bool cpu6502::LDY()
{
	memorize();
	y = memory;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return true;
}

/* Logical Shift Right
| Behaves the same way as the ASL but here we perform
| M >> 1 rather than M << 1 and sets a carry flag with
| the first bit of the memory value
*/
bool cpu6502::LSR()
{
	memorize();
	uint16_t res_ext = (uint16_t)memory >> 1;

	set_flag(flag_c, memory & 1);
	set_flag(flag_n, 0);

	uint8_t res = res_ext & 0x00FF;

	// Only applies to the accumulator
	set_flag(flag_z, res == 0);

	// We write to the accumulator if the addressing mode is implied
	// otherwise we write to the memory

	if (instructions[opcode].addr_mode == &IMP)
		accumulator = res;
	else
		write(abs_addr, res);

	return false;
}

/* No OPeration
| Does nothing and requires no additional
| clock cycles, does it?
*/
bool cpu6502::NOP()
{
	return false;
}

/* Bitwise OR
| Performs A = A | M and modifies the Z and N flags
*/
bool cpu6502::ORA()
{
	memorize();
	accumulator |= memory;

	set_flag(flag_z, accumulator == 0);
	set_flag(flag_n, accumulator & 0b10000000);

	return true;
}

/* PusH Accumulator
| Pushes the value of the accumulator to the stack
*/
bool cpu6502::PHA()
{
	push_stack(accumulator);
	return false;
}

/* PusH Processor status
| Pushes processor status to the stack
*/
bool cpu6502::PHP()
{
	push_stack(status | flag_b | flag_1);
	return false;
}

bool cpu6502::PLA()
{
	return false;
}

bool cpu6502::PLP()
{
	return false;
}

bool cpu6502::ROL()
{
	return false;
}

bool cpu6502::ROR()
{
	return false;
}

bool cpu6502::RTI()
{
	return false;
}

bool cpu6502::RTS()
{
	return false;
}

/* SuBtract with a Carry
| Performs a subtraction:
| in ADC: A = A + M + C, here: A = A - (1 - C) - M (*).
| Notice that we have (1 - C) instead of just C because C here is a borrow,
| Let's rewrite (*) so we can implement this instruction with the ADC instruction:
| A = A + (-M - 1) + C
*/
bool cpu6502::SBC()
{
	// Inversing M (memory) is quite trivial since we store all numbers as 2's complement
	// M becomes ~M + 1

	memorize();

	// ~M
	uint16_t inv = uint16_t(memory ^ 0xFF); // + 1 - 1

	// A + ~M + C
	uint16_t res = (uint16_t)memory + inv + (uint16_t)accumulator;
	if (get_flag(flag_c)) res++;

	// Other stuff is the same as in ADC

	// Overflow
	bool v = ((uint16_t)accumulator ^ res) & ~((uint16_t)accumulator ^ memory);
	set_flag(flag_v, v & 0b10000000);

	// Store the result (taking last 1 byte because the result is actually 1 byte in size)
	accumulator = res & 0x00FF;

	// Check for a 0 result
	set_flag(flag_z, accumulator == 0);

	// Set if there is something to carry because if there is
	// we will accumulate it on the next call of the ADC instruction
	set_flag(flag_c, res > 255);

	// If the first bit is set to 1 then we've got a negative number
	set_flag(flag_n, res & 0b10000000);

	return false;
}

bool cpu6502::SEC()
{
	return false;
}

bool cpu6502::SED()
{
	return false;
}

bool cpu6502::SEI()
{
	return false;
}

bool cpu6502::STA()
{
	return false;
}

bool cpu6502::STX()
{
	return false;
}

bool cpu6502::STY()
{
	return false;
}

bool cpu6502::TAX()
{
	return false;
}

bool cpu6502::TAY()
{
	return false;
}

bool cpu6502::TSX()
{
	return false;
}

bool cpu6502::TXA()
{
	return false;
}

bool cpu6502::TXS()
{
	return false;
}

bool cpu6502::TYA()
{
	return false;
}

/* INcrement X
| Performs X = X + 1 and sets Z and N flags
*/
bool cpu6502::INX()
{
	x++;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* INcrement Y
| Performs Y = Y + 1 and sets Z and N flags
*/
bool cpu6502::INY()
{
	y++;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

