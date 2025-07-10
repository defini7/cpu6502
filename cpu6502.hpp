#pragma once

#include <cstdint>
#include <vector>
#include <string>

#include "bus.hpp"

class Bus;

class CPU6502
{
public:
    CPU6502();
    
public:
    void reset();
    void clock();

    void connect_bus(Bus* bus);

    void write(uint16_t addr, uint8_t value);
    uint8_t read(uint16_t addr);

    bool get_flag(uint8_t flag) const;

    uint16_t get_abs_addr() const;
    uint16_t get_rel_addr() const;
    uint8_t get_accumulator() const;
    uint8_t get_x() const;
    uint8_t get_y() const;
    uint8_t get_stack_ptr() const;
    uint16_t get_pc() const;
    uint8_t get_cycles_count() const;

    struct Instruction;
    const Instruction& get_inst(uint8_t i) const;

protected:
    // Stores a number of clock cycles to execute
    uint8_t cycles = 0;

    // Stores the last fetched opcode
    uint8_t opcode = 0;

    // It's just a value from a RAM that's being set
    // based on the addressing mode so it can be
    // used later during the opcode execution stage
    uint8_t memory = 0;

    Bus* bus = nullptr;

    union
    {
        // Here is the address that we want to read from
        // during the opcode execution process,
        // this variable is modified by the addressing modes
        uint16_t abs_addr = 0;

        struct
        {
            uint8_t abs_addr_0;
            uint8_t abs_addr_1;
        };
    };

    // In 6502 CPU we have branch instructions that allow us to
    // jump a certain distance from the current location, for this
    // kind of situations we will use relative address
    uint16_t rel_addr = 0;

    /* 1) Registers: https://www.nesdev.org/wiki/CPU_registers */

    // Stores some intermidiate data, works with ALU
    uint8_t accumulator = 0;

    // Used for several addressing modes
    uint8_t x = 0, y = 0;

    // Pointer to a stack (in a RAM),
    // valid values are $100-$1FF
    uint8_t stack_ptr = 0;

    // Points to the next instruction to be executed
    // (also in a RAM)
    uint16_t program_counter = 0;

    // Each bit stores a flag (actually only 7 are used)
    /*
    *   7  bit  0
        ---- ----
        NV1B DIZC
        |||| ||||
        |||| |||+- Carry
        |||| ||+-- Zero
        |||| |+--- Interrupt Disable
        |||| +---- Decimal
        |||+------ (No CPU effect; see: the B flag)
        ||+------- (No CPU effect; always pushed as 1)
        |+-------- Overflow
        +--------- Negative
    */
    uint8_t status = 0;

public:

    /* 2) Status flags: https://www.nesdev.org/wiki/Status_flags */

    // Note that each enum value is not an index of a specific bit
    // that's an offset in bits that we use later to set the specific bit
    // and avoid additional shifting
    enum Flags : uint8_t
    {
        flag_c = 1 << 0, // carry
        flag_z = 1 << 1, // zero
        flag_i = 1 << 2, // disable interrupts
        flag_d = 1 << 3, // decimal mode (there is no support of floating point mode so just set it to 1)
        flag_b = 1 << 4, // unused by CPU
        flag_1 = 1 << 5, // unused by CPU, always 1
        flag_v = 1 << 6, // overflow
        flag_n = 1 << 7  // negative
    };

private:

    /* 3) Opcodes and addressing modes: https://www.nesdev.org/wiki/Instruction_reference */

    // True means that we need an additional clock cycle

    // Opcodes
    bool ADC();	bool AND();	bool ASL();	bool BCC();	bool BCS();	bool BEQ();	bool BIT();	bool BMI();	bool BNE();	bool BPL();
    bool BRK();	bool BVC();	bool BVS();	bool CLC(); bool CLD();	bool CLI();	bool CLV();	bool CMP();	bool CPX();	bool CPY();
    bool DEC();	bool DEX();	bool DEY();	bool EOR();	bool INC();	bool INX();	bool INY();	bool JMP(); bool JSR();	bool LDA();
    bool LDX();	bool LDY();	bool LSR();	bool NOP();	bool ORA();	bool PHA();	bool PHP();	bool PLA();	bool PLP();	bool ROL();
    bool ROR();	bool RTI(); bool RTS();	bool SBC();	bool SEC();	bool SED();	bool SEI();	bool STA();	bool STX();	bool STY();
    bool TAX();	bool TAY();	bool TSX();	bool TXA();	bool TXS();	bool TYA();

    // Used for an invalid opcode
    bool XXX();

    void cond_branch_base(uint8_t flag, bool value);

public:
    // Addressing modes: https://www.zophar.net/fileuploads/2/10532krzvs/6502.txt,
    // https://www.nesdev.org/obelisk-6502-guide/addressing.html
    bool IMM(); bool ABS(); bool ZPA();
    bool ZPX(); bool ZPY(); bool ABX();
    bool ABY(); bool IMP(); bool REL();
    bool IZX(); bool IZY(); bool IND();

private:
    struct Instruction
    {
        std::string name;
        
        bool (CPU6502::*opcode)() = nullptr;
        bool (CPU6502::*addr_mode)() = nullptr;

        uint8_t cycles;
    };

    std::vector<Instruction> instructions;

    /* 4) Interrupts: https://www.nesdev.org/wiki/CPU_interrupts */
    
    void IRQ();
    void NMI();

    // Utils

    void push_stack(uint8_t value);
    uint8_t pop_stack();

    void set_flag(uint8_t flag, bool enable);
    void memorize();

};
