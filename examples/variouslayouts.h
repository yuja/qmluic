#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class VariousLayouts;
}

namespace UiSupport {
class VariousLayouts;
}

class VariousLayouts : public QWidget
{
    Q_OBJECT

public:
    explicit VariousLayouts(QWidget *parent = nullptr);
    ~VariousLayouts() override;

private:
    std::unique_ptr<Ui::VariousLayouts> ui_;
    std::unique_ptr<UiSupport::VariousLayouts> uiSupport_;
};
