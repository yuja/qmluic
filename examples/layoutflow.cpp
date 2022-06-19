#include "layoutflow.h"
#include "ui_layoutflow.h"

LayoutFlow::LayoutFlow(QWidget *parent)
    : QWidget(parent)
    , ui_(std::make_unique<Ui::LayoutFlow>())
{
    ui_->setupUi(this);
}

LayoutFlow::~LayoutFlow() = default;
