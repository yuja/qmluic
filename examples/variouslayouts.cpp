#include "variouslayouts.h"
#include "ui_variouslayouts.h"

VariousLayouts::VariousLayouts(QWidget *parent)
    : QWidget(parent)
    , ui_(std::make_unique<Ui::VariousLayouts>())
{
    ui_->setupUi(this);
}

VariousLayouts::~VariousLayouts() = default;
