#include "../Include/bus.hpp"

void Bus::connect_cpu(cpu6502* cpu)
{
    m_cpu = cpu;

    // Provide our bus to the cpu
    m_cpu->connect_bus(this);
}

void Bus::write(uint16_t addr, uint8_t value)
{
    if (0 <= addr && addr < m_ram.size())
        m_ram[addr] = value;
}

uint8_t Bus::read(uint16_t addr)
{
    if (0 <= addr && addr < m_ram.size())
        return m_ram[addr];

    return 0;
}
