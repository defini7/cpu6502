#pragma once

#include "cpu6502.hpp"
#include <array>

class CPU6502;

class Bus
{
public:
    Bus() = default;

    void connect_cpu(CPU6502* cpu);
    uint32_t get_ram_size() const;

    void write(uint16_t addr, uint8_t value);
    uint8_t read(uint16_t addr);

protected:
    // There are only 2 devices that are connected to the bus

    CPU6502* m_cpu = nullptr;
    
    // RAM (8 KB)
    std::array<uint8_t, 8 * 1024 * 8> m_ram;

};
