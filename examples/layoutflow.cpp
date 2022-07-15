#include "layoutflow.h"
#include "ui_layoutflow.h"
#include "uisupport_layoutflow.h"

LayoutFlow::LayoutFlow(QWidget *parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::LayoutFlow>()),
      uiSupport_(std::make_unique<UiSupport::LayoutFlow>(this, ui_.get()))

{
    ui_->setupUi(this);
    uiSupport_->setup();
}

LayoutFlow::~LayoutFlow() = default;
