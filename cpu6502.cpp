#include "../Include/cpu6502.hpp"

cpu6502::cpu6502()
{
    instructions =
    {
		{ "BRK", &BRK, &IMM, 7 }, { "ORA", &ORA, &INX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "ORA", &ORA, &ZPA, 3 }, { "ASL", &ASL, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PHP", &PHP, &IMP, 3 }, { "ORA", &ORA, &IMM, 2 }, { "ASL", &ASL, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ABS, 4 }, { "ASL", &ASL, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BPL", &BPL, &REL, 2 }, { "ORA", &ORA, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ZPX, 4 }, { "ASL", &ASL, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLC", &CLC, &IMP, 2 }, { "ORA", &ORA, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "ORA", &ORA, &ABX, 4 }, { "ASL", &ASL, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "JSR", &JSR, &ABS, 6 }, { "AND", &AND, &INX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "BIT", &BIT, &ZPA, 3 }, { "AND", &AND, &ZPA, 3 }, { "ROL", &ROL, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PLP", &PLP, &IMP, 4 }, { "AND", &AND, &IMM, 2 }, { "ROL", &ROL, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "BIT", &BIT, &ABS, 4 }, { "AND", &AND, &ABS, 4 }, { "ROL", &ROL, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BMI", &BMI, &REL, 2 }, { "AND", &AND, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "AND", &AND, &ZPX, 4 }, { "ROL", &ROL, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SEC", &SEC, &IMP, 2 }, { "AND", &AND, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "AND", &AND, &ABX, 4 }, { "ROL", &ROL, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "RTI", &RTI, &IMP, 6 }, { "EOR", &EOR, &INX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "EOR", &EOR, &ZPA, 3 }, { "LSR", &LSR, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PHA", &PHA, &IMP, 3 }, { "EOR", &EOR, &IMM, 2 }, { "LSR", &LSR, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "JMP", &JMP, &ABS, 3 }, { "EOR", &EOR, &ABS, 4 }, { "LSR", &LSR, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BVC", &BVC, &REL, 2 }, { "EOR", &EOR, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "EOR", &EOR, &ZPX, 4 }, { "LSR", &LSR, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLI", &CLI, &IMP, 2 }, { "EOR", &EOR, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "EOR", &EOR, &ABX, 4 }, { "LSR", &LSR, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "RTS", &RTS, &IMP, 6 }, { "ADC", &ADC, &INX, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 3 }, { "ADC", &ADC, &ZPA, 3 }, { "ROR", &ROR, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "PLA", &PLA, &IMP, 4 }, { "ADC", &ADC, &IMM, 2 }, { "ROR", &ROR, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "JMP", &JMP, &IND, 5 }, { "ADC", &ADC, &ABS, 4 }, { "ROR", &ROR, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BVS", &BVS, &REL, 2 }, { "ADC", &ADC, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "ADC", &ADC, &ZPX, 4 }, { "ROR", &ROR, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SEI", &SEI, &IMP, 2 }, { "ADC", &ADC, &ABY, 4 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "ADC", &ADC, &ABX, 4 }, { "ROR", &ROR, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "###", &NOP, &IMP, 2 }, { "STA", &STA, &INX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 6 }, { "STY", &STY, &ZPA, 3 }, { "STA", &STA, &ZPA, 3 }, { "STX", &STX, &ZPA, 3 }, { "###", &XXX, &IMP, 3 }, { "DEY", &DEY, &IMP, 2 }, { "###", &NOP, &IMP, 2 }, { "TXA", &TXA, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "STY", &STY, &ABS, 4 }, { "STA", &STA, &ABS, 4 }, { "STX", &STX, &ABS, 4 }, { "###", &XXX, &IMP, 4 },
		{ "BCC", &BCC, &REL, 2 }, { "STA", &STA, &INY, 6 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 6 }, { "STY", &STY, &ZPX, 4 }, { "STA", &STA, &ZPX, 4 }, { "STX", &STX, &ZPY, 4 }, { "###", &XXX, &IMP, 4 }, { "TYA", &TYA, &IMP, 2 }, { "STA", &STA, &ABY, 5 }, { "TXS", &TXS, &IMP, 2 }, { "###", &XXX, &IMP, 5 }, { "###", &NOP, &IMP, 5 }, { "STA", &STA, &ABX, 5 }, { "###", &XXX, &IMP, 5 }, { "###", &XXX, &IMP, 5 },
		{ "LDY", &LDY, &IMM, 2 }, { "LDA", &LDA, &INX, 6 }, { "LDX", &LDX, &IMM, 2 }, { "###", &XXX, &IMP, 6 }, { "LDY", &LDY, &ZPA, 3 }, { "LDA", &LDA, &ZPA, 3 }, { "LDX", &LDX, &ZPA, 3 }, { "###", &XXX, &IMP, 3 }, { "TAY", &TAY, &IMP, 2 }, { "LDA", &LDA, &IMM, 2 }, { "TAX", &TAX, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "LDY", &LDY, &ABS, 4 }, { "LDA", &LDA, &ABS, 4 }, { "LDX", &LDX, &ABS, 4 }, { "###", &XXX, &IMP, 4 },
		{ "BCS", &BCS, &REL, 2 }, { "LDA", &LDA, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 5 }, { "LDY", &LDY, &ZPX, 4 }, { "LDA", &LDA, &ZPX, 4 }, { "LDX", &LDX, &ZPY, 4 }, { "###", &XXX, &IMP, 4 }, { "CLV", &CLV, &IMP, 2 }, { "LDA", &LDA, &ABY, 4 }, { "TSX", &TSX, &IMP, 2 }, { "###", &XXX, &IMP, 4 }, { "LDY", &LDY, &ABX, 4 }, { "LDA", &LDA, &ABX, 4 }, { "LDX", &LDX, &ABY, 4 }, { "###", &XXX, &IMP, 4 },
		{ "CPY", &CPY, &IMM, 2 }, { "CMP", &CMP, &INX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "CPY", &CPY, &ZPA, 3 }, { "CMP", &CMP, &ZPA, 3 }, { "DEC", &DEC, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "INY", &INY, &IMP, 2 }, { "CMP", &CMP, &IMM, 2 }, { "DEX", &DEX, &IMP, 2 }, { "###", &XXX, &IMP, 2 }, { "CPY", &CPY, &ABS, 4 }, { "CMP", &CMP, &ABS, 4 }, { "DEC", &DEC, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BNE", &BNE, &REL, 2 }, { "CMP", &CMP, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "CMP", &CMP, &ZPX, 4 }, { "DEC", &DEC, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "CLD", &CLD, &IMP, 2 }, { "CMP", &CMP, &ABY, 4 }, { "NOP", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "CMP", &CMP, &ABX, 4 }, { "DEC", &DEC, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
		{ "CPX", &CPX, &IMM, 2 }, { "SBC", &SBC, &INX, 6 }, { "###", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "CPX", &CPX, &ZPA, 3 }, { "SBC", &SBC, &ZPA, 3 }, { "INC", &INC, &ZPA, 5 }, { "###", &XXX, &IMP, 5 }, { "INX", &INX, &IMP, 2 }, { "SBC", &SBC, &IMM, 2 }, { "NOP", &NOP, &IMP, 2 }, { "###", &SBC, &IMP, 2 }, { "CPX", &CPX, &ABS, 4 }, { "SBC", &SBC, &ABS, 4 }, { "INC", &INC, &ABS, 6 }, { "###", &XXX, &IMP, 6 },
		{ "BEQ", &BEQ, &REL, 2 }, { "SBC", &SBC, &INY, 5 }, { "###", &XXX, &IMP, 2 }, { "###", &XXX, &IMP, 8 }, { "###", &NOP, &IMP, 4 }, { "SBC", &SBC, &ZPX, 4 }, { "INC", &INC, &ZPX, 6 }, { "###", &XXX, &IMP, 6 }, { "SED", &SED, &IMP, 2 }, { "SBC", &SBC, &ABY, 4 }, { "NOP", &NOP, &IMP, 2 }, { "###", &XXX, &IMP, 7 }, { "###", &NOP, &IMP, 4 }, { "SBC", &SBC, &ABX, 4 }, { "INC", &INC, &ABX, 7 }, { "###", &XXX, &IMP, 7 },
    };																																																																																	  
}

void cpu6502::reset()
{
	stack_ptr = 0x100;
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

/* Accumulator AM
| Simply saves a value from
| an accumulator into a memory
*/
bool cpu6502::ACC()
{
	memory = accumulator;
	return false;
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

	uint8_t old_left = abs_addr_0;
	abs_addr += y;

	if (abs_addr_0 != old_left)
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

bool cpu6502::REL()
{
	return false;
}

bool cpu6502::INX()
{
	return false;
}

bool cpu6502::INY()
{
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

	// Get the data we want
	abs_addr_0 = read(ptr + 1);
	abs_addr_1 = read(ptr + 0);

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

bool cpu6502::ADC()
{
	return false;
}

/* Binary AND
| Performs A = A & memory
| Sets Z and N flags
*/
bool cpu6502::AND()
{
	accumulator &= memory;
	
	set_flag(flag_z, accumulator == 0);

	// The value is negative if the leftmost bit is set to 1
	set_flag(flag_n, accumulator & 0b10000000);

	// requires additional clock cycle
	return true;
}

/* Arithmetic shift left
* Performs 
*/
bool cpu6502::ASL()
{
	
}

bool cpu6502::BCC()
{
	return false;
}

bool cpu6502::BCS()
{
	return false;
}

bool cpu6502::BEQ()
{
	return false;
}

bool cpu6502::BIT()
{
	return false;
}

bool cpu6502::BMI()
{
	return false;
}

bool cpu6502::BNE()
{
	return false;
}

bool cpu6502::BPL()
{
	return false;
}

bool cpu6502::BRK()
{
	return false;
}

bool cpu6502::BVC()
{
	return false;
}

bool cpu6502::BVS()
{
	return false;
}

bool cpu6502::CLC()
{
	return false;
}

bool cpu6502::CLD()
{
	return false;
}

bool cpu6502::CLI()
{
	return false;
}

bool cpu6502::CLV()
{
	return false;
}

bool cpu6502::CMP()
{
	return false;
}

bool cpu6502::CPX()
{
	return false;
}

bool cpu6502::CPY()
{
	return false;
}

bool cpu6502::DEC()
{
	return false;
}

bool cpu6502::DEX()
{
	return false;
}

bool cpu6502::DEY()
{
	return false;
}

bool cpu6502::EOR()
{
	return false;
}

bool cpu6502::INC()
{
	return false;
}

bool cpu6502::XXX()
{
	return false;
}

bool cpu6502::JMP()
{
	return false;
}

bool cpu6502::JSR()
{
	return false;
}

bool cpu6502::LDA()
{
	return false;
}

bool cpu6502::LDX()
{
	return false;
}

bool cpu6502::LDY()
{
	return false;
}

bool cpu6502::LSR()
{
	return false;
}

bool cpu6502::NOP()
{
	return false;
}

bool cpu6502::ORA()
{
	return false;
}

bool cpu6502::PHA()
{
	return false;
}

bool cpu6502::PHP()
{
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

bool cpu6502::SBC()
{
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
