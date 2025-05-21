<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\DatabaseMigrationService;

class MappingRule extends \Google\Model
{
  protected $conditionalColumnSetValueType = ConditionalColumnSetValue::class;
  protected $conditionalColumnSetValueDataType = '';
  protected $convertRowidColumnType = ConvertRowIdToColumn::class;
  protected $convertRowidColumnDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $entityMoveType = EntityMove::class;
  protected $entityMoveDataType = '';
  protected $filterType = MappingRuleFilter::class;
  protected $filterDataType = '';
  protected $filterTableColumnsType = FilterTableColumns::class;
  protected $filterTableColumnsDataType = '';
  protected $multiColumnDataTypeChangeType = MultiColumnDatatypeChange::class;
  protected $multiColumnDataTypeChangeDataType = '';
  protected $multiEntityRenameType = MultiEntityRename::class;
  protected $multiEntityRenameDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $revisionCreateTime;
  /**
   * @var string
   */
  public $revisionId;
  /**
   * @var string
   */
  public $ruleOrder;
  /**
   * @var string
   */
  public $ruleScope;
  protected $setTablePrimaryKeyType = SetTablePrimaryKey::class;
  protected $setTablePrimaryKeyDataType = '';
  protected $singleColumnChangeType = SingleColumnChange::class;
  protected $singleColumnChangeDataType = '';
  protected $singleEntityRenameType = SingleEntityRename::class;
  protected $singleEntityRenameDataType = '';
  protected $singlePackageChangeType = SinglePackageChange::class;
  protected $singlePackageChangeDataType = '';
  protected $sourceSqlChangeType = SourceSqlChange::class;
  protected $sourceSqlChangeDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param ConditionalColumnSetValue
   */
  public function setConditionalColumnSetValue(ConditionalColumnSetValue $conditionalColumnSetValue)
  {
    $this->conditionalColumnSetValue = $conditionalColumnSetValue;
  }
  /**
   * @return ConditionalColumnSetValue
   */
  public function getConditionalColumnSetValue()
  {
    return $this->conditionalColumnSetValue;
  }
  /**
   * @param ConvertRowIdToColumn
   */
  public function setConvertRowidColumn(ConvertRowIdToColumn $convertRowidColumn)
  {
    $this->convertRowidColumn = $convertRowidColumn;
  }
  /**
   * @return ConvertRowIdToColumn
   */
  public function getConvertRowidColumn()
  {
    return $this->convertRowidColumn;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param EntityMove
   */
  public function setEntityMove(EntityMove $entityMove)
  {
    $this->entityMove = $entityMove;
  }
  /**
   * @return EntityMove
   */
  public function getEntityMove()
  {
    return $this->entityMove;
  }
  /**
   * @param MappingRuleFilter
   */
  public function setFilter(MappingRuleFilter $filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return MappingRuleFilter
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param FilterTableColumns
   */
  public function setFilterTableColumns(FilterTableColumns $filterTableColumns)
  {
    $this->filterTableColumns = $filterTableColumns;
  }
  /**
   * @return FilterTableColumns
   */
  public function getFilterTableColumns()
  {
    return $this->filterTableColumns;
  }
  /**
   * @param MultiColumnDatatypeChange
   */
  public function setMultiColumnDataTypeChange(MultiColumnDatatypeChange $multiColumnDataTypeChange)
  {
    $this->multiColumnDataTypeChange = $multiColumnDataTypeChange;
  }
  /**
   * @return MultiColumnDatatypeChange
   */
  public function getMultiColumnDataTypeChange()
  {
    return $this->multiColumnDataTypeChange;
  }
  /**
   * @param MultiEntityRename
   */
  public function setMultiEntityRename(MultiEntityRename $multiEntityRename)
  {
    $this->multiEntityRename = $multiEntityRename;
  }
  /**
   * @return MultiEntityRename
   */
  public function getMultiEntityRename()
  {
    return $this->multiEntityRename;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setRevisionCreateTime($revisionCreateTime)
  {
    $this->revisionCreateTime = $revisionCreateTime;
  }
  /**
   * @return string
   */
  public function getRevisionCreateTime()
  {
    return $this->revisionCreateTime;
  }
  /**
   * @param string
   */
  public function setRevisionId($revisionId)
  {
    $this->revisionId = $revisionId;
  }
  /**
   * @return string
   */
  public function getRevisionId()
  {
    return $this->revisionId;
  }
  /**
   * @param string
   */
  public function setRuleOrder($ruleOrder)
  {
    $this->ruleOrder = $ruleOrder;
  }
  /**
   * @return string
   */
  public function getRuleOrder()
  {
    return $this->ruleOrder;
  }
  /**
   * @param string
   */
  public function setRuleScope($ruleScope)
  {
    $this->ruleScope = $ruleScope;
  }
  /**
   * @return string
   */
  public function getRuleScope()
  {
    return $this->ruleScope;
  }
  /**
   * @param SetTablePrimaryKey
   */
  public function setSetTablePrimaryKey(SetTablePrimaryKey $setTablePrimaryKey)
  {
    $this->setTablePrimaryKey = $setTablePrimaryKey;
  }
  /**
   * @return SetTablePrimaryKey
   */
  public function getSetTablePrimaryKey()
  {
    return $this->setTablePrimaryKey;
  }
  /**
   * @param SingleColumnChange
   */
  public function setSingleColumnChange(SingleColumnChange $singleColumnChange)
  {
    $this->singleColumnChange = $singleColumnChange;
  }
  /**
   * @return SingleColumnChange
   */
  public function getSingleColumnChange()
  {
    return $this->singleColumnChange;
  }
  /**
   * @param SingleEntityRename
   */
  public function setSingleEntityRename(SingleEntityRename $singleEntityRename)
  {
    $this->singleEntityRename = $singleEntityRename;
  }
  /**
   * @return SingleEntityRename
   */
  public function getSingleEntityRename()
  {
    return $this->singleEntityRename;
  }
  /**
   * @param SinglePackageChange
   */
  public function setSinglePackageChange(SinglePackageChange $singlePackageChange)
  {
    $this->singlePackageChange = $singlePackageChange;
  }
  /**
   * @return SinglePackageChange
   */
  public function getSinglePackageChange()
  {
    return $this->singlePackageChange;
  }
  /**
   * @param SourceSqlChange
   */
  public function setSourceSqlChange(SourceSqlChange $sourceSqlChange)
  {
    $this->sourceSqlChange = $sourceSqlChange;
  }
  /**
   * @return SourceSqlChange
   */
  public function getSourceSqlChange()
  {
    return $this->sourceSqlChange;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MappingRule::class, 'Google_Service_DatabaseMigrationService_MappingRule');
