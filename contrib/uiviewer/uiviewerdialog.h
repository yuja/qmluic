#ifndef UIVIEWERDIALOG_H
#define UIVIEWERDIALOG_H

#include <QDialog>
#include <QWidget>
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

    void setContentWidget(std::unique_ptr<QWidget> widget);

protected:
    bool eventFilter(QObject *watched, QEvent *event) override;

private slots:
    void reparentContentWidget();

private:
    std::unique_ptr<Ui::UiViewerDialog> ui_;
    std::unique_ptr<QWidget> contentWidget_;
    Qt::WindowFlags contentWindowFlags_;
};

#endif // UIVIEWERDIALOG_H
