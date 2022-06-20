#include "mydialogbuttonbox.h"
#include "ui_mydialogbuttonbox.h"

MyDialogButtonBox::MyDialogButtonBox(QWidget *parent)
    : QDialogButtonBox(parent), ui_(std::make_unique<Ui::MyDialogButtonBox>())
{
    ui_->setupUi(this);
}

MyDialogButtonBox::~MyDialogButtonBox() = default;
