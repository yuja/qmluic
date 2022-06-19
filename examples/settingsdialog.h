#pragma once

#include <QDialog>
#include <memory>

namespace Ui { class SettingsDialog; }

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:
    SettingsDialog(QWidget *parent = nullptr);
    ~SettingsDialog() override;

private:
    std::unique_ptr<Ui::SettingsDialog> ui_;
};
