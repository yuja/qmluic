#ifndef UIVIEWERDIALOG_H
#define UIVIEWERDIALOG_H

#include <QDialog>
#include <memory>

namespace Ui {
class UiViewerDialog;
}

class UiViewerDialog : public QDialog
{
    Q_OBJECT

public:
    explicit UiViewerDialog(QWidget *parent = nullptr);
    ~UiViewerDialog() override;

private:
    std::unique_ptr<Ui::UiViewerDialog> ui_;
};

#endif // UIVIEWERDIALOG_H
