-- Data cleaning using SQL

Select *
From [Portfolio Project].dbo.NashvilleHousing

--Standardize Date Format

Select SaleDate, CONVERT(date,SaleDate)
From [Portfolio Project].dbo.NashvilleHousing

Alter Table NashvilleHousing
Add SaleDateConverted Date;

Update NashvilleHousing
Set SaleDateConverted = CONVERT(Date,SaleDate)

Select SaleDateConverted
From NashvilleHousing

--Populate property address data

Select PropertyAddress
From [Portfolio Project].dbo.NashvilleHousing
Where PropertyAddress is null

Select *
From [Portfolio Project].dbo.NashvilleHousing
--Where PropertyAddress is null
Order by ParcelID

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress
From [Portfolio Project].dbo.NashvilleHousing a
JOIN [Portfolio Project].dbo.NashvilleHousing b
	on a.ParcelID = b.parcelID
	and a.[UniqueID] <> b.[UniqueID]
Where a.PropertyAddress is null

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From [Portfolio Project].dbo.NashvilleHousing a
JOIN [Portfolio Project].dbo.NashvilleHousing b
	on a.ParcelID = b.parcelID
	and a.[UniqueID] <> b.[UniqueID]
Where a.PropertyAddress is null

Update a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From [Portfolio Project].dbo.NashvilleHousing a
JOIN [Portfolio Project].dbo.NashvilleHousing b
	on a.ParcelID = b.parcelID
	and a.[UniqueID] <> b.[UniqueID]
Where a.PropertyAddress is null

--Double check by running the second to last chunk of code

-- Breaking out Address into seperate columns (Address, City, State)

Select PropertyAddress
From [Portfolio Project].dbo.NashvilleHousing

Select
SUBSTRING(PropertyAddress, 1, CHARINDEX(',',PropertyAddress)-1) as Address
From [Portfolio Project].dbo.NashvilleHousing

Select
SUBSTRING(PropertyAddress, 1, CHARINDEX(',',PropertyAddress)-1) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',',PropertyAddress)+1, LEN(PropertyAddress)) as City
From [Portfolio Project].dbo.NashvilleHousing

--Now we will alter the table

Alter Table NashvilleHousing
Add PropertySplitAddress Nvarchar(255);

Update NashvilleHousing
Set PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',',PropertyAddress)-1)

Alter Table NashvilleHousing
Add PropertySplitCity Nvarchar(255);

Update NashvilleHousing
Set PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',',PropertyAddress)+1, LEN(PropertyAddress))

--Check to make sure this worked as desired

Select *
From [Portfolio Project].dbo.NashvilleHousing

--Now we'll do the owner address which also contains the state

Select OwnerAddress
From [Portfolio Project].dbo.NashvilleHousing

Select
PARSENAME(REPLACE(OwnerAddress, ',', '.'),3),
PARSENAME(REPLACE(OwnerAddress, ',', '.'),2),
PARSENAME(REPLACE(OwnerAddress, ',', '.'),1)
From [Portfolio Project].dbo.NashvilleHousing

--Now we can add the new columns for the seperated values

Alter Table NashvilleHousing
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.'),3)

Alter Table NashvilleHousing
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.'),2)

Alter Table NashvilleHousing
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.'),1)

--Check to make sure this worked
Select *
From [Portfolio Project].dbo.NashvilleHousing

--Change 1 and 0 to Yes and No in "Sold as Vacant" field

Select Distinct(SoldAsVacant), Count(SoldAsVacant)
From [Portfolio Project].dbo.NashvilleHousing
Group by SoldAsVacant
Order by 2

Select SoldAsVacant,
Case When Cast(SoldAsVacant as Varchar) = '1' Then 'Yes'
When Cast(SoldAsVacant as Varchar) = '0' Then 'No'
Else Cast(SoldAsVacant as Varchar)
End
From [Portfolio Project].dbo.NashvilleHousing

ALTER TABLE NashvilleHousing
ALTER COLUMN SoldAsVacant NVARCHAR(10);

Update NashvilleHousing
Set SoldAsVacant = Case When Cast(SoldAsVacant as Varchar) = '1' Then 'Yes'
When Cast(SoldAsVacant as Varchar) = '0' Then 'No'
Else Cast(SoldAsVacant as Varchar)
End

Select SoldAsVacant
From [Portfolio Project].dbo.NashvilleHousing

--Remove Duplicates
WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() Over(
	Partition by ParcelID,
	PropertyAddress,
	SalePrice,
	SaleDate,
	LegalReference
	Order by
		UniqueID
		) row_num
From [Portfolio Project].dbo.NashvilleHousing
--Order by row_num desc
)
Delete
From RowNumCTE
Where row_num >1
--Order by PropertyAddress

--Below is to check that this worked as intended. It should yield no results
WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() Over(
	Partition by ParcelID,
	PropertyAddress,
	SalePrice,
	SaleDate,
	LegalReference
	Order by
		UniqueID
		) row_num
From [Portfolio Project].dbo.NashvilleHousing
--Order by row_num desc
)
Select * 
From RowNumCTE
Where row_num > 1

--Delete unused columns, specifically those containing address information that we adjusted earlier

Select *
From [Portfolio Project].dbo.NashvilleHousing

Alter Table NashvilleHousing
Drop Column OwnerAddress, PropertyAddress

--Also dropped the original date column
Alter Table NashvilleHousing
Drop Column SaleDate