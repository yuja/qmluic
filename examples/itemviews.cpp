#include <QDir>
#include <QFileSystemModel>
#include "itemviews.h"
#include "ui_itemviews.h"

ItemViews::ItemViews(QWidget *parent)
    : QWidget(parent),
      ui_(std::make_unique<Ui::ItemViews>()),
      fsModel_(std::make_unique<QFileSystemModel>())
{
    ui_->setupUi(this);

    // Install a model to render views. Here we use QFileSystemModel because it's easy.
    ui_->treeView->setModel(fsModel_.get());
    ui_->tableView->setModel(fsModel_.get());
    auto homeIndex = fsModel_->setRootPath(QDir::homePath());
    ui_->treeView->setRootIndex(homeIndex);
    ui_->tableView->setRootIndex(homeIndex);
    connect(ui_->treeView, &QTreeView::activated, this, [this](const QModelIndex &index) {
        ui_->tableView->setRootIndex(fsModel_->isDir(index) ? index : index.parent());
    });
}

ItemViews::~ItemViews() = default;
