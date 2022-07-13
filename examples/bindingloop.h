#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class BindingLoop;
}

namespace UiSupport {
class BindingLoop;
}

class BindingLoop : public QWidget
{
    Q_OBJECT

public:
    explicit BindingLoop(QWidget *parent = nullptr);
    ~BindingLoop() override;

private:
    std::unique_ptr<Ui::BindingLoop> ui_;
    std::unique_ptr<UiSupport::BindingLoop> uiSupport_;
};
