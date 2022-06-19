#include "staticitemmodel.h"
#include "ui_staticitemmodel.h"

StaticItemModel::StaticItemModel(QWidget *parent)
    : QWidget(parent), ui_(std::make_unique<Ui::StaticItemModel>())
{
    ui_->setupUi(this);
}

StaticItemModel::~StaticItemModel() = default;
