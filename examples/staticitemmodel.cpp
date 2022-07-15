#include "staticitemmodel.h"
#include "ui_staticitemmodel.h"
#include "uisupport_staticitemmodel.h"

StaticItemModel::StaticItemModel(QWidget *parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::StaticItemModel>()),
      uiSupport_(std::make_unique<UiSupport::StaticItemModel>(this, ui_.get()))
{
    ui_->setupUi(this);
    uiSupport_->setup();
}

StaticItemModel::~StaticItemModel() = default;
