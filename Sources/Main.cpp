#include "../Include/cpu6502.hpp"
#include "defGameEngine.hpp"

#include <strstream>

std::string to_hex(uint32_t value, int8_t len)
{
    static const char* alphabet = "0123456789ABCDEF";
    std::string res; res.resize(len);

    for (int8_t i = len - 1; i >= 0; i--, value >>= 4)
        res[i] = alphabet[value & 0x0F];

    return res;
}

class Test : public def::GameEngine
{
public:
    Test()
    {
        GetWindow()->SetTitle("Test CPU6502");
    }

    CPU6502 cpu;
    Bus bus;

    uint16_t program_start = 0x8000;

    std::unordered_map<uint16_t, std::string> program;

    void disassemble()
    {
        uint16_t program_end = program_start + program_size - 1;
        uint16_t addr = program_start;

        program.clear();

        while (addr <= program_end)
        {
            std::stringstream inst;

            uint16_t line = addr;
            uint8_t opcode = cpu.read(addr++);

            const auto& inst_info = cpu.get_inst(opcode);
            inst << "$" << to_hex(line, 4) << ' ' << inst_info.name;

            if (inst_info.addr_mode == &CPU6502::IMP)
                inst << " {IMP}";
            else if (inst_info.addr_mode == &CPU6502::IMM)
            {
                uint8_t data = cpu.read(addr++);
                inst << " #$" << to_hex(data, 2) << " {IMM}";
            }
            else if (inst_info.addr_mode == &CPU6502::ZPA)
            {
                uint8_t data = cpu.read(addr++);
                inst << " $" << to_hex(data, 2) << " {ZPA}";
            }
            else if (inst_info.addr_mode == &CPU6502::ZPX)
            {
                uint8_t data = cpu.read(addr++);
                inst << " $" << to_hex(data, 2) << ", X {ZPX}";
            }
            else if (inst_info.addr_mode == &CPU6502::ZPY)
            {
                uint8_t data = cpu.read(addr++);
                inst << " $" << to_hex(data, 2) << ", Y {ZPY}";
            }
            else if (inst_info.addr_mode == &CPU6502::REL)
            {
                uint8_t data = cpu.read(addr++);
                inst << " $" << to_hex(data, 2) << " {REL}";
            }
            else if (inst_info.addr_mode == &CPU6502::ABS)
            {
                uint8_t low = cpu.read(addr++);
                uint8_t high = cpu.read(addr++);
                inst << " $" << to_hex(high, 2) << to_hex(low, 2) << " {ABS}";
            }
            else if (inst_info.addr_mode == &CPU6502::ABX)
            {
                uint8_t low = cpu.read(addr++);
                uint8_t high = cpu.read(addr++);
                inst << " $" << to_hex(high, 2) << to_hex(low, 2) << ", X {ABX}";
            }
            else if (inst_info.addr_mode == &CPU6502::ABY)
            {
                uint8_t low = cpu.read(addr++);
                uint8_t high = cpu.read(addr++);
                inst << " $" << to_hex(high, 2) << to_hex(low, 2) << ", Y {ABY}";
            }
            else if (inst_info.addr_mode == &CPU6502::IND)
            {
                uint8_t low = cpu.read(addr++);
                uint8_t high = cpu.read(addr++);
                inst << " ($" << to_hex(high, 2) << to_hex(low, 2) << ") {IND}";
            }
            else if (inst_info.addr_mode == &CPU6502::IZX)
            {
                uint8_t ptr = cpu.read(addr++);
                inst << " ($" << to_hex(ptr, 2) << ", X) {IZX}";
            }
            else if (inst_info.addr_mode == &CPU6502::IZY)
            {
                uint8_t ptr = cpu.read(addr++);
                inst << " ($" << to_hex(ptr, 2) << "), Y {IZY}";
            }
            else
                std::cout << "Can't disassemble an instruction at $" << to_hex(line, 4) << std::endl;

            program[line] = inst.str();
        }
    }

    // Drawing the program with the center at PC
    void draw_program(int x, int y, int pc, int offset)
    {
        uint16_t program_end = program_start + program_size;
        int oy = 0;

        for (int i = pc - offset; i < pc + offset; i++)
        {
            def::Pixel col = (i == pc) ? def::BLUE : def::WHITE;

            if (program.contains(i))
                DrawString(x, y + oy, program.at(i), col);
            else
                DrawString(x, y + oy, "$0000 XXX", col);

            oy += 10;
        }
    }

    uint16_t program_size = 0;

protected:
    bool OnUserCreate() override
    {
        bus.connect_cpu(&cpu);

        uint16_t addr = program_start;

        // I use this to convert assembly code into object code:
        // https://www.masswerk.at/6502/assembler.html

        /*
        SEC            ; Set carry
        LDA #$50
        SBC #$30       ; A = $50 - $30 = $20
        */

        std::stringstream code;
        code << "38 A9 50 E9 30";

        while (!code.eof())
        {
            std::string byte;
            code >> byte;
            cpu.write(addr++, std::stoi(byte, nullptr, 16));

            program_size++;
        }

        disassemble();

        // Providing a program counter
        cpu.write(0xFFFC, program_start & 0x00FF);
        cpu.write(0xFFFD, (program_start >> 8) & 0x00FF);

        cpu.reset();

        return true;
    }

    bool OnUserUpdate(float) override
    {
        auto i = GetInput();

        if (i->GetKeyState(def::Key::SPACE).released)
        {
            do cpu.clock();
            while (cpu.get_cycles_count() != 0);
        }

        if (i->GetKeyState(def::Key::R).released)
            cpu.reset();

        Clear(def::BLACK);

        // Draw flags

        int x = 0;
        DrawString(x += 12, 20, "C", cpu.get_flag(CPU6502::flag_c) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "Z", cpu.get_flag(CPU6502::flag_z) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "I", cpu.get_flag(CPU6502::flag_i) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "D", cpu.get_flag(CPU6502::flag_d) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "B", cpu.get_flag(CPU6502::flag_b) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "1", cpu.get_flag(CPU6502::flag_1) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "V", cpu.get_flag(CPU6502::flag_v) ? def::GREEN : def::RED);
        DrawString(x += 12, 20, "N", cpu.get_flag(CPU6502::flag_n) ? def::GREEN : def::RED);

        // Draw registers

        int y = 20;
        DrawString(12, y += 12, "Abs addr: $" + to_hex(cpu.get_abs_addr(), 4));
        DrawString(12, y += 12, "Rel addr: $" + to_hex(cpu.get_rel_addr(), 4));
        DrawString(12, y += 12, "Accumulator: $" + to_hex(cpu.get_accumulator(), 2));
        DrawString(12, y += 12, "X: $" + to_hex(cpu.get_x(), 2));
        DrawString(12, y += 12, "Y: $" + to_hex(cpu.get_y(), 2));
        DrawString(12, y += 12, "Stack ptr: $" + to_hex(cpu.get_stack_ptr(), 2));
        DrawString(12, y += 12, "PC: $" + to_hex(cpu.get_pc(), 4));
        DrawString(12, y += 12, "Cycles: $" + to_hex(cpu.get_cycles_count(), 2));

        // Draw program
        draw_program(200, 50, cpu.get_pc(), 20);

        return true;
    }

};

int main()
{
    Test app;

    if (app.Construct(512, 480, 2, 2))
        app.Run();
}
