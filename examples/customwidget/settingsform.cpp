#include "settingsform.h"
#include "ui_settingsform.h"

SettingsForm::SettingsForm(QWidget *parent)
    : QWidget(parent), ui_(std::make_unique<Ui::SettingsForm>())
{
    ui_->setupUi(this);
}

SettingsForm::~SettingsForm() = default;
