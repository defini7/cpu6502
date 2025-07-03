#pragma once

#include "cpu6502.hpp"
#include <array>

class Bus
{
public:
    Bus() = default;

    void connect_cpu(cpu6502* cpu);
    
    void write(uint16_t addr, uint8_t value);
    uint8_t read(uint16_t addr);

protected:
    // There are only 2 devices that are connected to the bus

    // CPU 6502
    cpu6502* m_cpu = nullptr;
    
    // RAM (2 KB)
    std::array<uint8_t, 2 * 1024 * 8> m_ram;

};
