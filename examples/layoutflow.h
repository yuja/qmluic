#pragma once

#include <QWidget>
#include <memory>

namespace Ui { class LayoutFlow; }

class LayoutFlow : public QWidget
{
    Q_OBJECT

public:
    LayoutFlow(QWidget *parent = nullptr);
    ~LayoutFlow() override;

private:
    std::unique_ptr<Ui::LayoutFlow> ui_;
};
