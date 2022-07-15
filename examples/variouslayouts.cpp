#include "variouslayouts.h"
#include "ui_variouslayouts.h"
#include "uisupport_variouslayouts.h"

VariousLayouts::VariousLayouts(QWidget *parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::VariousLayouts>()),
      uiSupport_(std::make_unique<UiSupport::VariousLayouts>(this, ui_.get()))
{
    ui_->setupUi(this);
    uiSupport_->setup();
}

VariousLayouts::~VariousLayouts() = default;
