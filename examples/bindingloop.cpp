#include "bindingloop.h"
#include "ui_bindingloop.h"
#include "uisupport_bindingloop.h"

BindingLoop::BindingLoop(QWidget *parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::BindingLoop>()),
      uiSupport_(std::make_unique<UiSupport::BindingLoop>(this, ui_.get()))

{
    ui_->setupUi(this);
    uiSupport_->setup();
}

BindingLoop::~BindingLoop() = default;
