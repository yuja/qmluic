#pragma once

#include <QDialog>
#include <memory>

namespace Ui {
class SettingsDialog;
}

namespace UiSupport {
class SettingsDialog;
}

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SettingsDialog(QWidget *parent = nullptr);
    ~SettingsDialog() override;

private:
    std::unique_ptr<Ui::SettingsDialog> ui_;
    std::unique_ptr<UiSupport::SettingsDialog> uiSupport_;
};
