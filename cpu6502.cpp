#include "../Include/CPU6502.hpp"

CPU6502::CPU6502()
{
	using c = CPU6502;

    instructions =
    {
		{ "BRK", &c::BRK, &c::IMM, 7 }, { "ORA", &c::ORA, &c::IZX, 6 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 3 }, { "ORA", &c::ORA, &c::ZPA, 3 }, { "ASL", &c::ASL, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "PHP", &c::PHP, &c::IMP, 3 }, { "ORA", &c::ORA, &c::IMM, 2 }, { "ASL", &c::ASL, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::NOP, &c::IMP, 4 }, { "ORA", &c::ORA, &c::ABS, 4 }, { "ASL", &c::ASL, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BPL", &c::BPL, &c::REL, 2 }, { "ORA", &c::ORA, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "ORA", &c::ORA, &c::ZPX, 4 }, { "ASL", &c::ASL, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "CLC", &c::CLC, &c::IMP, 2 }, { "ORA", &c::ORA, &c::ABY, 4 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "ORA", &c::ORA, &c::ABX, 4 }, { "ASL", &c::ASL, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
		{ "JSR", &c::JSR, &c::ABS, 6 }, { "AND", &c::AND, &c::IZX, 6 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "BIT", &c::BIT, &c::ZPA, 3 }, { "AND", &c::AND, &c::ZPA, 3 }, { "ROL", &c::ROL, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "PLP", &c::PLP, &c::IMP, 4 }, { "AND", &c::AND, &c::IMM, 2 }, { "ROL", &c::ROL, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "BIT", &c::BIT, &c::ABS, 4 }, { "AND", &c::AND, &c::ABS, 4 }, { "ROL", &c::ROL, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BMI", &c::BMI, &c::REL, 2 }, { "AND", &c::AND, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "AND", &c::AND, &c::ZPX, 4 }, { "ROL", &c::ROL, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "SEC", &c::SEC, &c::IMP, 2 }, { "AND", &c::AND, &c::ABY, 4 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "AND", &c::AND, &c::ABX, 4 }, { "ROL", &c::ROL, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
		{ "RTI", &c::RTI, &c::IMP, 6 }, { "EOR", &c::EOR, &c::IZX, 6 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 3 }, { "EOR", &c::EOR, &c::ZPA, 3 }, { "LSR", &c::LSR, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "PHA", &c::PHA, &c::IMP, 3 }, { "EOR", &c::EOR, &c::IMM, 2 }, { "LSR", &c::LSR, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "JMP", &c::JMP, &c::ABS, 3 }, { "EOR", &c::EOR, &c::ABS, 4 }, { "LSR", &c::LSR, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BVC", &c::BVC, &c::REL, 2 }, { "EOR", &c::EOR, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "EOR", &c::EOR, &c::ZPX, 4 }, { "LSR", &c::LSR, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "CLI", &c::CLI, &c::IMP, 2 }, { "EOR", &c::EOR, &c::ABY, 4 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "EOR", &c::EOR, &c::ABX, 4 }, { "LSR", &c::LSR, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
		{ "RTS", &c::RTS, &c::IMP, 6 }, { "ADC", &c::ADC, &c::IZX, 6 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 3 }, { "ADC", &c::ADC, &c::ZPA, 3 }, { "ROR", &c::ROR, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "PLA", &c::PLA, &c::IMP, 4 }, { "ADC", &c::ADC, &c::IMM, 2 }, { "ROR", &c::ROR, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "JMP", &c::JMP, &c::IND, 5 }, { "ADC", &c::ADC, &c::ABS, 4 }, { "ROR", &c::ROR, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BVS", &c::BVS, &c::REL, 2 }, { "ADC", &c::ADC, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "ADC", &c::ADC, &c::ZPX, 4 }, { "ROR", &c::ROR, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "SEI", &c::SEI, &c::IMP, 2 }, { "ADC", &c::ADC, &c::ABY, 4 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "ADC", &c::ADC, &c::ABX, 4 }, { "ROR", &c::ROR, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
		{ "###", &c::NOP, &c::IMP, 2 }, { "STA", &c::STA, &c::IZX, 6 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 6 }, { "STY", &c::STY, &c::ZPA, 3 }, { "STA", &c::STA, &c::ZPA, 3 }, { "STX", &c::STX, &c::ZPA, 3 }, { "###", &c::XXX, &c::IMP, 3 }, { "DEY", &c::DEY, &c::IMP, 2 }, { "###", &c::NOP, &c::IMP, 2 }, { "TXA", &c::TXA, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "STY", &c::STY, &c::ABS, 4 }, { "STA", &c::STA, &c::ABS, 4 }, { "STX", &c::STX, &c::ABS, 4 }, { "###", &c::XXX, &c::IMP, 4 },
		{ "BCC", &c::BCC, &c::REL, 2 }, { "STA", &c::STA, &c::IZY, 6 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 6 }, { "STY", &c::STY, &c::ZPX, 4 }, { "STA", &c::STA, &c::ZPX, 4 }, { "STX", &c::STX, &c::ZPY, 4 }, { "###", &c::XXX, &c::IMP, 4 }, { "TYA", &c::TYA, &c::IMP, 2 }, { "STA", &c::STA, &c::ABY, 5 }, { "TXS", &c::TXS, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 5 }, { "###", &c::NOP, &c::IMP, 5 }, { "STA", &c::STA, &c::ABX, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "###", &c::XXX, &c::IMP, 5 },
		{ "LDY", &c::LDY, &c::IMM, 2 }, { "LDA", &c::LDA, &c::IZX, 6 }, { "LDX", &c::LDX, &c::IMM, 2 }, { "###", &c::XXX, &c::IMP, 6 }, { "LDY", &c::LDY, &c::ZPA, 3 }, { "LDA", &c::LDA, &c::ZPA, 3 }, { "LDX", &c::LDX, &c::ZPA, 3 }, { "###", &c::XXX, &c::IMP, 3 }, { "TAY", &c::TAY, &c::IMP, 2 }, { "LDA", &c::LDA, &c::IMM, 2 }, { "TAX", &c::TAX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "LDY", &c::LDY, &c::ABS, 4 }, { "LDA", &c::LDA, &c::ABS, 4 }, { "LDX", &c::LDX, &c::ABS, 4 }, { "###", &c::XXX, &c::IMP, 4 },
		{ "BCS", &c::BCS, &c::REL, 2 }, { "LDA", &c::LDA, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 5 }, { "LDY", &c::LDY, &c::ZPX, 4 }, { "LDA", &c::LDA, &c::ZPX, 4 }, { "LDX", &c::LDX, &c::ZPY, 4 }, { "###", &c::XXX, &c::IMP, 4 }, { "CLV", &c::CLV, &c::IMP, 2 }, { "LDA", &c::LDA, &c::ABY, 4 }, { "TSX", &c::TSX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 4 }, { "LDY", &c::LDY, &c::ABX, 4 }, { "LDA", &c::LDA, &c::ABX, 4 }, { "LDX", &c::LDX, &c::ABY, 4 }, { "###", &c::XXX, &c::IMP, 4 },
		{ "CPY", &c::CPY, &c::IMM, 2 }, { "CMP", &c::CMP, &c::IZX, 6 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "CPY", &c::CPY, &c::ZPA, 3 }, { "CMP", &c::CMP, &c::ZPA, 3 }, { "DEC", &c::DEC, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "IZY", &c::IZY, &c::IMP, 2 }, { "CMP", &c::CMP, &c::IMM, 2 }, { "DEX", &c::DEX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 2 }, { "CPY", &c::CPY, &c::ABS, 4 }, { "CMP", &c::CMP, &c::ABS, 4 }, { "DEC", &c::DEC, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BNE", &c::BNE, &c::REL, 2 }, { "CMP", &c::CMP, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "CMP", &c::CMP, &c::ZPX, 4 }, { "DEC", &c::DEC, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "CLD", &c::CLD, &c::IMP, 2 }, { "CMP", &c::CMP, &c::ABY, 4 }, { "NOP", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "CMP", &c::CMP, &c::ABX, 4 }, { "DEC", &c::DEC, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
		{ "CPX", &c::CPX, &c::IMM, 2 }, { "SBC", &c::SBC, &c::IZX, 6 }, { "###", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "CPX", &c::CPX, &c::ZPA, 3 }, { "SBC", &c::SBC, &c::ZPA, 3 }, { "INC", &c::INC, &c::ZPA, 5 }, { "###", &c::XXX, &c::IMP, 5 }, { "IZX", &c::IZX, &c::IMP, 2 }, { "SBC", &c::SBC, &c::IMM, 2 }, { "NOP", &c::NOP, &c::IMP, 2 }, { "###", &c::SBC, &c::IMP, 2 }, { "CPX", &c::CPX, &c::ABS, 4 }, { "SBC", &c::SBC, &c::ABS, 4 }, { "INC", &c::INC, &c::ABS, 6 }, { "###", &c::XXX, &c::IMP, 6 },
		{ "BEQ", &c::BEQ, &c::REL, 2 }, { "SBC", &c::SBC, &c::IZY, 5 }, { "###", &c::XXX, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 8 }, { "###", &c::NOP, &c::IMP, 4 }, { "SBC", &c::SBC, &c::ZPX, 4 }, { "INC", &c::INC, &c::ZPX, 6 }, { "###", &c::XXX, &c::IMP, 6 }, { "SED", &c::SED, &c::IMP, 2 }, { "SBC", &c::SBC, &c::ABY, 4 }, { "NOP", &c::NOP, &c::IMP, 2 }, { "###", &c::XXX, &c::IMP, 7 }, { "###", &c::NOP, &c::IMP, 4 }, { "SBC", &c::SBC, &c::ABX, 4 }, { "INC", &c::INC, &c::ABX, 7 }, { "###", &c::XXX, &c::IMP, 7 },
    };
}

void CPU6502::reset()
{
	stack_ptr = 0xFD;
	program_counter = 0;
	accumulator = 0;
	x = 0; y = 0;

	abs_addr = 0;
	rel_addr = 0;
	memory = 0;

	// Reset takes 7 cycles itself
	cycles = 7;

	program_counter = (read(0xFFFD) << 8) | read(0xFFFC);
}

void CPU6502::clock()
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

void CPU6502::connect_bus(Bus* bus)
{
	this->bus = bus;
}

void CPU6502::write(uint16_t addr, uint8_t value)
{
	bus->write(addr, value);
}

uint8_t CPU6502::read(uint16_t addr)
{
	return bus->read(addr);
}

void CPU6502::push_stack(uint8_t value)
{
	write(0x100 + (uint16_t)stack_ptr, value);
	stack_ptr--;
}

uint8_t CPU6502::pop_stack()
{
	stack_ptr++;
	return read(0x100 + (uint16_t)stack_ptr);
}

void CPU6502::memorize()
{
	// We want to memorize data for every addressing mode
	// except the IMPlied one because there is nothing
	// to memorize

	if (instructions[opcode].addr_mode != &CPU6502::IMP)
		memory = read(abs_addr);
}

/* Immediate AM
| The data is supplied as part of the instruction,
| so it stores the address of the next byte
| (of the argument of the instruction)
*/
bool CPU6502::IMM()
{
	abs_addr = program_counter++;
	return false;
}

/* Absolute AM
| Here we specify the full 16-bit address of the data to use.
*/
bool CPU6502::ABS()
{
	// Read 8 rightmost bits
	abs_addr_1 = read(program_counter++);

	// Read 8 leftmost bits
	abs_addr_0 = read(program_counter++);

	return false;
}

/* Pages
| Note that we use 8KB of RAM connected to the bus,
| 8KB = 256 * 256 bytes so the idea is to split
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
bool CPU6502::ZPA()
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
bool CPU6502::ZPX()
{
	abs_addr_0 = 0x00;
	abs_addr_1 = read(program_counter++) + x;

	return false;
}

/* Zero Page Y AM
| The same thing as ZPA but also use Y register to offset the offset
*/
bool CPU6502::ZPY()
{
	abs_addr_0 = 0x00;
	abs_addr_1 = read(program_counter++) + y;

	return false;
}

/* Absolute X AM
| Here we specify the full 16-bit address of the data to use
| and offset it by the X register
*/
bool CPU6502::ABX()
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
bool CPU6502::ABY()
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
bool CPU6502::IMP()
{
	memory = accumulator;
	return false;
}

/* Relative AM
| Only applied to the branching instructions (e.g. JMP), so it must
| set relative address to jump from the current location in the program
| to another
*/
bool CPU6502::REL()
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
bool CPU6502::IZX()
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
bool CPU6502::IZY()
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
bool CPU6502::IND()
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

void CPU6502::set_flag(uint8_t flag, bool enable)
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

bool CPU6502::get_flag(uint8_t flag) const
{
	return (status & flag) != 0;
}

uint16_t CPU6502::get_abs_addr() const
{
	return abs_addr;
}

uint16_t CPU6502::get_rel_addr() const
{
	return rel_addr;
}

uint8_t CPU6502::get_accumulator() const
{
	return accumulator;
}

uint8_t CPU6502::get_x() const
{
	return x;
}

uint8_t CPU6502::get_y() const
{
	return y;
}

uint8_t CPU6502::get_stack_ptr() const
{
	return stack_ptr;
}

uint16_t CPU6502::get_pc() const
{
	return program_counter;
}

uint8_t CPU6502::get_cycles_count() const
{
	return cycles;
}

const CPU6502::Instruction& CPU6502::get_inst(uint8_t i) const
{
	return instructions[i];
}

/* ADd with a Carry
| Simly performs addition. However, it is not as easy as it sounds:
| you need to keep in mind that you can work with signed and unsigned numbers
| and of course they have their own numeric borders, so this operation
| changes negative (n), overflow (v) flags and zero (z) flags
*/
bool CPU6502::ADC()
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
bool CPU6502::AND()
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
bool CPU6502::ASL()
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

	if (instructions[opcode].addr_mode == &CPU6502::IMP)
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
void CPU6502::cond_branch_base(uint8_t flag, bool value)
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
bool CPU6502::BCC()
{
	cond_branch_base(flag_c, false);
	return false;
}

/* Branch if Carry Set
| If the carry flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BCS()
{
	cond_branch_base(flag_c, true);
	return false;
}

/* Branch if Equal
| If the zero flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BEQ()
{
	cond_branch_base(flag_z, true);
	return false;
}

/* Bit Test
| This instruction doesn't change the memory or the registers,
| it changes Z, V and N flags
*/
bool CPU6502::BIT()
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
bool CPU6502::BMI()
{
	cond_branch_base(flag_n, true);
	return false;
}

/* Branch if Not Equal
| If the zero flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BNE()
{
	cond_branch_base(flag_z, false);
	return false;
}

/* Branch if Positive
| If the negative flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BPL()
{
	cond_branch_base(flag_n, false);
	return false;
}

bool CPU6502::BRK()
{
	return false;
}

/* Branch if Overflow Clear
| If the overflow flag is set to 0 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BVC()
{
	cond_branch_base(flag_v, false);
	return false;
}

/* Branch if Overflow Set
| If the overflow flag is set to 1 then we jump
| by an offset (rel_addr) and it requires 1 additional cycle
| if we don't cross the page boundary and requires 2 if we cross it
*/
bool CPU6502::BVS()
{
	cond_branch_base(flag_v, true);
	return false;
}

/* Clear Carry Flag
| Sets the carry flag to 0
*/
bool CPU6502::CLC()
{
	set_flag(flag_c, false);
	return false;
}

/* Clear Decimal Mode
| Sets the decimal mode flag to 0
*/
bool CPU6502::CLD()
{
	set_flag(flag_d, false);
	return false;
}

/* Clear Interrupt Disable
| Sets the interrupt disable flag to 0
*/
bool CPU6502::CLI()
{
	set_flag(flag_i, false);
	return false;
}

/* Clear Overflow Flag
| Sets the overflow flag to 0
*/
bool CPU6502::CLV()
{
	set_flag(flag_v, false);
	return false;
}

/* CoMPare A
| Simply compares values of the accumulator and of the memory
| by performing A - M and then it modifies carry, zero and negative flags,
| notice that it does not change a value of anything
*/
bool CPU6502::CMP()
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
bool CPU6502::CPX()
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
bool CPU6502::CPY()
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
bool CPU6502::DEC()
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
bool CPU6502::DEX()
{
	x--;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* DEcrement Y
| Decrements a value of the Y register and sets Z and N flags
*/
bool CPU6502::DEY()
{
	y--;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

/* Exclusive OR
| Performs A = A ^ M and modifies the Z and N flags
*/
bool CPU6502::EOR()
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
bool CPU6502::INC()
{
	memorize();

	uint8_t res = memory + 1;
	write(abs_addr, res);

	set_flag(flag_z, res == 0);
	set_flag(flag_n, res & 0b10000000);

	return false;
}

bool CPU6502::XXX()
{
	// Nothing to do
	return false;
}

void CPU6502::IRQ()
{}

void CPU6502::NMI()
{}

/* JuMP
| Jumping to the specified address
*/
bool CPU6502::JMP()
{
	// We have precalculated the address in the addressing mode
	program_counter = abs_addr; 
	return false;
}

/* Jump to SubRoutine
| Pushes the pointer to the previous instruction to the stack and
| jumps to the new address
*/
bool CPU6502::JSR()
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
bool CPU6502::LDA()
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
bool CPU6502::LDX()
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
bool CPU6502::LDY()
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
bool CPU6502::LSR()
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

	if (instructions[opcode].addr_mode == &CPU6502::IMP)
		accumulator = res;
	else
		write(abs_addr, res);

	return false;
}

/* No OPeration
| Does nothing and requires no additional
| clock cycles, does it?
*/
bool CPU6502::NOP()
{
	return false;
}

/* Bitwise OR
| Performs A = A | M and modifies the Z and N flags
*/
bool CPU6502::ORA()
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
bool CPU6502::PHA()
{
	push_stack(accumulator);
	return false;
}

/* PusH Processor status
| Pushes processor status to the stack
*/
bool CPU6502::PHP()
{
	push_stack(status | flag_b | flag_1);
	return false;
}

/* PuLl Accumulator
| Pops a value from a stack and stores it in the accumulator
*/
bool CPU6502::PLA()
{
	accumulator = pop_stack();

	set_flag(flag_z, accumulator == 0);
	set_flag(flag_n, accumulator & 0b10000000);

	return false;
}

/* PuLl Processor status
| Pops a value from a stack and stores it in the status register
*/
bool CPU6502::PLP()
{
	status = pop_stack();
	set_flag(flag_1, true);
	return false;
}

/* ROtate Left
| Shifts one bit of the memory to the left and storing it in
| the accumulator or in the memory
*/
bool CPU6502::ROL()
{
	memorize();

	uint16_t carry = get_flag(flag_c);

	// Setting the low byte of the result to 1 if there is something to carry
	uint16_t res = uint16_t(memory << 1) | carry;

	uint8_t res_low = res & 0x00FF;

	set_flag(flag_c, memory & 0b10000000);
	set_flag(flag_z, res_low == 0);
	set_flag(flag_n, res_low & 0b10000000);

	if (instructions[opcode].addr_mode == &CPU6502::IMP)
		accumulator = res_low;
	else
		write(abs_addr, res_low);

	return false;
}

/* ROtate Right
| Shifts one bit of the memory to the right and storing it in
| the accumulator or in the memory
*/
bool CPU6502::ROR()
{
	memorize();

	uint16_t carry = get_flag(flag_c);

	// Setting a high byte of the result to 1 if there is something to carry
	uint16_t res = uint16_t(memory >> 1) | (carry << 7);

	uint8_t res_low = res & 0x00FF;

	// If the low byte is set to 1 then there is something to carry
	set_flag(flag_c, memory & 1);
	set_flag(flag_z, res_low == 0);
	set_flag(flag_n, res_low & 0b10000000);

	if (instructions[opcode].addr_mode == &CPU6502::IMP)
		accumulator = res_low;
	else
		write(abs_addr, res_low);

	return false;
}

bool CPU6502::RTI()
{
	return false;
}

bool CPU6502::RTS()
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
bool CPU6502::SBC()
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

	// We can treat a carry as an overflow of the unsigned char value
	set_flag(flag_c, res > 255);

	// If the first bit is set to 1 then we've got a negative number
	set_flag(flag_n, res & 0b10000000);

	return false;
}

/* SEt Carry
| Sets the value of the carry flag to 1
*/
bool CPU6502::SEC()
{
	set_flag(flag_c, true);
	return false;
}

/* SEt Decimal
| Sets the value of the decimal flag to 1
*/
bool CPU6502::SED()
{
	set_flag(flag_d, true);
	return false;
}

/* SEt Interrupt disable
| Sets the value of the interrupt disable flag to 1
*/
bool CPU6502::SEI()
{
	set_flag(flag_i, true);
	return false;
}

/* STore Accumulator
| Stores the value of the accumulator into the memory
*/
bool CPU6502::STA()
{
	write(abs_addr, accumulator);
	return false;
}

/* STore X
| Stores the value of the X register into the memory
*/
bool CPU6502::STX()
{
	write(abs_addr, x);
	return false;
}

/* STore Y
| Stores the value of the Y register into the memory
*/
bool CPU6502::STY()
{
	write(abs_addr, y);
	return false;
}

/* Transfer A to X
| Transfers the value of the accumulator to the X register
*/
bool CPU6502::TAX()
{
	x = accumulator;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* Transfer Accumulator to Y
| Transfers the value of the accumulator to the Y register
*/
bool CPU6502::TAY()
{
	y = accumulator;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

/* Transfer Stack pointer to X
| Transfers the value of the stack pointer to the X register
*/
bool CPU6502::TSX()
{
	x = stack_ptr;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* Transfer X to Accumlator
| Transfers the value of the X register to the accumulator
*/
bool CPU6502::TXA()
{
	accumulator = x;
	return false;
}

/* Transfer X to Stack pointer
| Transfers the value of the stack pointer to the X register
*/
bool CPU6502::TXS()
{
	stack_ptr = x;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* Transfer Y to Accumlator
| Transfers the value of the Y register to the accumulator
*/
bool CPU6502::TYA()
{
	accumulator = y;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

/* INcrement X
| Performs X = X + 1 and sets Z and N flags
*/
bool CPU6502::INX()
{
	x++;

	set_flag(flag_z, x == 0);
	set_flag(flag_n, x & 0b10000000);

	return false;
}

/* INcrement Y
| Performs Y = Y + 1 and sets Z and N flags
*/
bool CPU6502::INY()
{
	y++;

	set_flag(flag_z, y == 0);
	set_flag(flag_n, y & 0b10000000);

	return false;
}

