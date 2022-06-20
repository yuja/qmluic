#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class SettingsForm;
}

class SettingsForm : public QWidget
{
    Q_OBJECT

public:
    SettingsForm(QWidget *parent = nullptr);
    ~SettingsForm() override;

private:
    std::unique_ptr<Ui::SettingsForm> ui_;
};
